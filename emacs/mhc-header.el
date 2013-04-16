;;; -*- mode: Emacs-Lisp; coding: utf-8 -*-

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Created: 2000/05/11
;; Revised: $Date$


;;; Commentary:

;; This file is a part of MHC, includes functions to manipulate
;; headers.


;;; Code:

;; Global Variable:

(defconst mhc-header-table
  '(("x-sc-day"         "X-SC-Day:"             mhc-parse/day)
    ("x-sc-cond"        "X-SC-Cond:"            mhc-parse/cond)
    ("x-sc-duration"    "X-SC-Duration:"        mhc-parse/duration)
    ("x-sc-subject"     "X-SC-Subject:"         mhc-parse/subject)
    ("x-sc-location"    "X-SC-Location:"        mhc-parse/location)
    ("x-sc-time"        "X-SC-Time:"            mhc-parse/time)
    ("x-sc-alarm"       "X-SC-Alarm:"           mhc-parse/alarm)
    ("x-sc-category"    "X-SC-Category:"        mhc-parse/category)
    ("x-sc-recurrence-tag" "X-SC-Recurrence-Tag:" mhc-parse/recurrence-tag)
;    ("x-sc-todo"       "X-SC-ToDo:"            mhc-parse/todo)
    ("x-sc-priority"    "X-SC-Priority:"        mhc-parse/priority)
    ("x-sc-record-id"   "X-SC-Record-Id:"       mhc-parse/record-id)
    ("x-sc-schedule"    "X-SC-Schdule:"         mhc-parse/schedule)
    ;; For backward compatibility
    ("x-sc-date"        "X-SC-Date:"            mhc-parse/old-style-date)
    ;; FIXME: 要削除
    ("x-sc-next"        "X-SC-Next:"            mhc-parse/next)))


(defmacro mhc-header-list ()
  "Return headers which are referenced by MHC."
  `(mapcar (lambda (a) (nth 1 a)) mhc-header-table))


(defmacro mhc-header-parse-function (key)
  "Return a function to parse KEY."
  `(nth 2 (assoc (downcase ,key) mhc-header-table)))


(defmacro mhc-header-narrowing (&rest form)
  "Evaluate FORM with restriction of editing in this buffer to the header."
  `(save-excursion
     (save-restriction
       (goto-char (point-min))
       (re-search-forward
        (concat "^" (regexp-quote mail-header-separator) "$\\|^$") nil t)
       (narrow-to-region (point-min) (match-beginning 0))
       (goto-char (point-min))
       ,@form)))
(put 'mhc-header-narrowing 'lisp-indent-function 0)
(put 'mhc-header-narrowing 'edebug-form-spec '(form body))


(defsubst mhc-header-goto-end ()
  "Move point at end of this header."
  (while (and
          (forward-line 1)
          (memq (following-char) '(?  ?\t)))))


(defun mhc-header-delete-header (header &optional regexp) "\
Remove HEADER in the narrowed buffer.
If REGEXP, HEADER is a regular expression."
  (save-excursion
    (let ((case-fold-search t)
          (regexp (if regexp header (concat "^" (regexp-quote header) ":"))))
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (mhc-header-goto-end)
        (delete-region (match-beginning 0) (point))))))


(defun mhc-header-put-value (header value)
  "Overwrite VALUE of HEADER in the narrowed buffer."
  (if (assoc (downcase header) mhc-header-table)
      (setq header
            (substring (nth 1 (assoc (downcase header) mhc-header-table)) 0 -1)))
  (let ((case-fold-search t)
        (regexp (concat "^" (regexp-quote header) ":")))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward regexp nil t)
          (save-restriction
            (mhc-header-goto-end)
            (delete-region (match-beginning 0) (point))
            (insert (format "%s: %s\n" header value))
            (narrow-to-region (point) (point-max))
            (mhc-header-delete-header header))
        (goto-char (point-max))
        (insert (format "%s: %s\n" header value))))))


(defun mhc-header-get-value (header &optional repeat)
  "Return value of HEADER in the narrowed buffer."
  (let ((point (point))
        (case-fold-search t)
        (regexp (concat "^" (regexp-quote header) ":[ \t]*"))
        value)
    (goto-char (point-min))
    (while (and (not value)
                (re-search-forward regexp nil t repeat))
      (mhc-header-goto-end)
      (setq value (buffer-substring-no-properties (match-end 0) (1- (point)))))
    (goto-char point)
    value))


(defun mhc-header-valid-p (header &optional repeat)
  "Valid HEADER in the narrowed buffer."
  (let ((get (mhc-header-get-value header repeat)))
    (and (stringp get) (not (string= "" get)))))


(defun mhc-header-delete-separator ()
  "Delete separator between header and body in this buffer."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^-*$" nil t)
        (delete-region (match-beginning 0) (match-end 0)))))



(provide 'mhc-header)

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

;;; mhc-header.el ends here.
