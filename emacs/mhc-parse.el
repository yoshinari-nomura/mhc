;;; -*- mode: Emacs-Lisp; coding: utf-8 -*-

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Created: 2000/04/30
;; Revised: $Date$


;;; Commentary:

;; This file is a part of MHC, and includes functions to parse
;; schedule headers.


;;; Code:

(require 'mhc-logic)
(require 'mhc-record)
(require 'mhc-header)

(defvar mhc-parse/strict nil)

(defun mhc-parse/continuous-lines ()
  "ヘッダの継続行を処理して、内容のみを取り出す関数"
  (let (list)
    (skip-chars-forward " \t\n")
    (while (not (eobp))
      (setq list
            (cons (buffer-substring-no-properties
                   (point)
                   (progn (end-of-line) (skip-chars-backward " \t") (point)))
                  list))
      (end-of-line)
      (skip-chars-forward " \t\n"))
    (mapconcat 'identity (nreverse list) " ")))

(defun mhc-parse/day (record schedule)
  (mhc-logic-parse-day (mhc-schedule-condition schedule))
  schedule)

(defun mhc-parse/cond (record schedule)
  (mhc-logic-parse-cond (mhc-schedule-condition schedule))
  schedule)

(defun mhc-parse/duration (record schedule)
  (mhc-logic-parse-duration (mhc-schedule-condition schedule))
  schedule)

;; FIXME: Need to be deleted.
(defun mhc-parse/todo (record schedule)
  (mhc-logic-parse-todo (mhc-schedule-condition schedule))
  schedule)

(defun mhc-parse/priority (record schedule)
  (if (looking-at mhc-logic/space-regexp)
      (goto-char (match-end 0)))
  (let ((content (buffer-substring
                  (point)
                  (progn (skip-chars-forward "0-9") (point)))))
    (if (looking-at mhc-logic/space-regexp)
        (goto-char (match-end 0)))
    (if (eobp)
        (mhc-schedule/set-priority schedule
                                   (if (eq (length content) 0)
                                       nil
                                     (string-to-number content)))
      (error "Parse ERROR !!!(at X-SC-Priority:)")))
  schedule)

(defun mhc-parse/subject (record schedule)
  (mhc-schedule/set-subject
   schedule
   (mhc-eword-decode-string (mhc-parse/continuous-lines)))
  schedule)

(defun mhc-parse/location (record schedule)
  (mhc-schedule/set-location
   schedule
   (mhc-eword-decode-string (mhc-parse/continuous-lines)))
  schedule)

(defconst mhc-parse/time-regexp "\\([012][0-9]\\):\\([0-5][0-9]\\)")

(defun mhc-parse/time (record schedule)
  (let ((time (mhc-parse/continuous-lines))
        begin end)
    (cond
     ((string-match (concat "^" mhc-parse/time-regexp "-" mhc-parse/time-regexp "$") time)
      (setq begin (+ (* 60 (string-to-number (match-string 1 time)))
                     (string-to-number (match-string 2 time)))
            end (+ (* 60 (string-to-number (match-string 3 time)))
                   (string-to-number (match-string 4 time)))))
     ((string-match (concat "^" mhc-parse/time-regexp "-?$") time)
      (setq begin (+ (* 60 (string-to-number (match-string 1 time)))
                     (string-to-number (match-string 2 time)))))
     ((string-match (concat "^-" mhc-parse/time-regexp "$") time)
      (setq end (+ (* 60 (string-to-number (match-string 1 time)))
                   (string-to-number (match-string 2 time)))))
     ((and mhc-parse/strict (not (string= "" time)))
      (error "Parse ERROR!!!(at X-SC-Time:)")))
    (mhc-schedule/set-time schedule begin end))
  schedule)

;; For backward compatibility.
(defun mhc-parse/old-style-date (record schedule)
  (mhc-logic-parse-old-style-date (mhc-schedule-condition schedule))
  (mhc-parse/time record schedule))

(defconst mhc-parse/alarm-regexp "^[0-9]+ \\(minute\\|hour\\|day\\)$")

(defun mhc-parse/alarm (record schedule)
  (let ((alarm (mhc-parse/continuous-lines)))
    (unless (or (not mhc-parse/strict)
                (string-match mhc-parse/alarm-regexp alarm)
                (string= "" alarm))
      (error "Parse ERROR!!! (at X-SC-Alarm:)"))
    (mhc-schedule/set-alarm schedule alarm))
  schedule)

(defun mhc-parse/category (record schedule)
  (let ((category (mhc-parse/continuous-lines)))
    (mhc-schedule/set-categories
     schedule
     (nconc (delq nil
                  (mapcar
                   (lambda (str)
                     (and (stringp str) (downcase str)))
                   (mhc-misc-split
                    (mhc-eword-decode-string category)
                    "[ \t]+")))
            (mhc-schedule-categories schedule))))
  (mhc-logic/set-todo (mhc-schedule-condition schedule)
                      (mhc-schedule-in-category-p schedule "todo"))
  schedule)


(defun mhc-parse/recurrence-tag (record schedule)
  (mhc-schedule/set-recurrence-tag
   schedule
   (mhc-eword-decode-string (mhc-parse/continuous-lines)))
  schedule)

;; FIXME: 要削除
(defun mhc-parse/next (record schedule)
  (let ((new (mhc-schedule-new record)))
    (if schedule (mhc-schedule/set-region-end schedule (point-min)))
    (mhc-schedule/set-region-start new (point-min))
    new))

;; FIXME: X-SC-Schedule の入れ子構造は、(mhc-db-add-exception-rule) の
;; 実装の都合上受け入れられないので、top level 以外の X-SC-Schedule は
;; 安全に無視される必要がある。
(defun mhc-parse/schedule (record schedule)
  (let ((buffer (current-buffer))
        (start (point))
        (end (point-max))
        (schedule (mhc-schedule-new record)))
    (mhc-schedule/set-region-start schedule start)
    (mhc-schedule/set-region-start schedule end)
    (with-temp-buffer
      (insert-buffer-substring buffer start end)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((start (point)))
          (if (skip-chars-forward " \t\n")
              (delete-region start (point))))
        (while (if (eobp)
                   nil
                 (eq ?\\ (progn (end-of-line) (preceding-char))))
          (delete-char -1)
          (forward-line))
        (forward-line))
      (goto-char (point-min))
      (mhc-parse/internal-parser record schedule)))
  schedule)

;; FIXME: top level 以外の場所で記述された X-SC-Record-Id: は安全に無
;; 視される必要があるが、現在の実装では何も考えずに上書きしてしまう。
(defun mhc-parse/record-id (record schedule)
  (mhc-record-set-id record (mhc-parse/continuous-lines))
  schedule)

;; FIXME: top level とそれ以外の場所で許される header が異なるので、
;; multi pass parser に組み替えるべきかも知れない。
(defun mhc-parse/internal-parser (record &optional schedule strict)
  "Internal parseser of schedule headers in this narrowed buffer."
  (let ((mhc-parse/strict strict)
        (case-fold-search t)
        func)
    (while (not (eobp))
      (if (looking-at "\\([^ \t:]+\\):")
          (progn
            (setq func (mhc-header-parse-function
                        (format "%s" (match-string 1))))
            (mhc-header-goto-end)
            (if (fboundp func)
                (save-restriction
                  (narrow-to-region (match-beginning 0) (point))
                  (goto-char (match-end 0))
                  (setq schedule
                        (funcall func
                                 record
                                 (or schedule
                                     (if (memq func '(mhc-parse/schedule mhc-parse/next))
                                         nil
                                       (mhc-parse/next record nil)))))
                  (goto-char (point-max)))))
        ;; Always skip non-header lines.
        (forward-line 1))))
  schedule)

(defun mhc-parse-buffer (&optional record strict)
  "Parse schedule headers in this buffer."
  (unless record
    (setq record (mhc-record-new (buffer-file-name))))
  (mhc-header-narrowing
    (let ((schedule (mhc-parse/internal-parser record nil strict)))
      (if schedule (mhc-schedule/set-region-end schedule (point)))))
  ;; 得られた構造を整理する
  (let (schedules sexp)
    ;; 現れた順序に直しておく
    (mhc-record-set-schedules record (nreverse (mhc-record-schedules record)))
    ;; 先頭のスケジュールをデフォルトとして参照して、欠けている要素を埋めておく
    (setq schedules (cdr (mhc-record-schedules record)))
    (while schedules
      (mhc-schedule-append-default (car schedules) (car (mhc-record-schedules record)))
      (setq schedules (cdr schedules)))
    ;; 各スケジュールの条件式を生成する
    (mhc-logic-compile-file record))
  record)

(defun mhc-parse-file (filename)
  "Parse schedules headers in the file, FILENAME."
  (save-excursion
    (set-buffer (mhc-get-buffer-create " *mhc-parse-file*"))
    (delete-region (point-min) (point-max))
    (mhc-insert-file-contents-as-coding-system mhc-default-coding-system filename)
    (mhc-parse-buffer (mhc-record-new filename))))



(provide 'mhc-parse)

;;; Copyright Notice:

;; Copyright (C) 1999, 2000 Yoshinari Nomura. All rights reserved.
;; Copyright (C) 2000 MHC developing team. All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS''
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL
;; THE TEAM OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;;; mhc-parse.el ends here.
