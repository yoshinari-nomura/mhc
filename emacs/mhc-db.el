;;; -*- mode: Emacs-Lisp; coding: utf-8 -*-

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Created: 2000/05/01
;; Revised: $Date$


;;; Commentary:

;; This file is a part of MHC, and includes functions to manipulate
;; database of schedules.


;;; Code:

(require 'mhc-day)
(require 'mhc-process)
(require 'mhc-schedule)

(defun mhc-db-scan (b e &optional nosort category search)
  (mhc-process-send-command
   (format "scan --format=emacs %04d%02d%02d-%04d%02d%02d%s%s"
           (mhc-date-yy b)
           (mhc-date-mm b)
           (mhc-date-dd b)
           (mhc-date-yy e)
           (mhc-date-mm e)
           (mhc-date-dd e)
           (if category  (format " --category=%s" category) "")
           (if search  (format " --search='%s'" search) ""))))

(defun mhc-db-search (&rest query)
  (let ((b (mhc-date-new 1970 1 1))
        (e (mhc-date-yy+ (mhc-date-now) 10)))
    (mhc-db-scan b e nil nil (mhc-db/query-to-search-string query))))

(defun mhc-db/quote-string (string)
  (format "\"%s\"" string))

(defun mhc-db/keyword-to-string (keyword)
  (format "%s" keyword))

(defun mhc-db/query-to-search-string (query)
  (let ((keywords '(:subject :body :category :recurrence_tag)) string)
    (mapconcat 'identity
               (delq nil
                     (mapcar
                      (lambda (keyword)
                        (if (setq string (plist-get query keyword))
                            (format "%s:%s" (substring (symbol-name keyword) 1)
                                    (mhc-db/quote-string string))))
                      keywords))
               " | ")))

(defun mhc-db-scan-month (year month &optional nosort category)
  (let ((first-date (mhc-date-new year month 1)))
    (mhc-db-scan first-date
                 (mhc-date-mm-last first-date)
                 nosort
                 category)))

(defun mhc-db-add-record-from-buffer (record buffer &optional force-refile)
  (let* ((slot (mhc-logic-record-to-slot record))
         (directory (and slot
                         (file-name-as-directory
                          (expand-file-name
                           "spool" (mhc-config-base-directory)))))
         (old-record))
    (unless slot (error "Cannot get schedule slot"))
    (if (mhc-record-name record)
        ;; Modifying existing record
        (setq old-record record)
      ;; Creating new record
      (mhc-record-set-name record (mhc-misc-get-new-path directory record)))
    (if (or force-refile
            (y-or-n-p (format
                       "Refile %s to %s "
                       (or (mhc-record-name old-record) "it")
                       (mhc-record-name record))))
        (progn
          (mhc-record-write-buffer record buffer old-record)
          (if (and old-record
                   (not (eq record old-record)))
              (let* ((dir (file-name-directory
                           (directory-file-name
                            (mhc-record-name old-record)))))
                (mhc-misc-touch-directory dir)))
          (mhc-misc-touch-directory directory)
          t))))


(defun mhc-db-delete-file (record)
  (let* ((dir (file-name-directory (directory-file-name (mhc-record-name record)))))
    (mhc-record-delete record)
    (mhc-misc-touch-directory dir)))

;; FIXME: X-SC-Schedule ヘッダによって指定された子スケジュールに対する
;; 例外規則の追加が動作しない。
(defun mhc-db-add-exception-rule (original-record except-day)
  (let ((date-string (mhc-day-let except-day
                       (format "%04d%02d%02d" year month day-of-month))))
    (with-temp-buffer
      (mhc-draft-reedit-file (mhc-record-name original-record))
      (let (record dayinfo schedule)
        (while (setq record (mhc-parse-buffer)
                     dayinfo (mhc-logic-eval-for-date (list (mhc-record-sexp record)) except-day)
                     schedule (car (mhc-day-schedules dayinfo)))
          (save-restriction
            (narrow-to-region (mhc-schedule-region-start schedule)
                              (mhc-schedule-region-end schedule))
            (mhc-header-put-value
             "x-sc-day"
             (mapconcat 'identity
                        (cons (format "!%s" date-string)
                              (delete date-string
                                      (mhc-logic-day-as-string-list
                                       (mhc-schedule-condition schedule))))
                        " "))))
        (mhc-record-set-name record (mhc-record-name original-record))
        (mhc-db-add-record-from-buffer record (current-buffer))))))



(provide 'mhc-db)

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

;;; mhc-db.el ends here.
