;;; mhc-db.el --- Database Interface to MHC.

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
(require 'mhc-record)
(require 'mhc-date)

(defun mhc-db-scan (begin-date end-date &optional nosort category search)
  "Scan MHC database from BEGIN-DATE to END-DATE.
If optional NOSORT is non-nil, returned value is not sort.
If optional CATEGORY is non-nil, returned value is clipped by category.
If optional SEARCH is non-nil returned value is clipped by search string."
  (mhc-process-send-command
   (format "scan --format=emacs %04d%02d%02d-%04d%02d%02d%s%s"
           (mhc-date-yy begin-date)
           (mhc-date-mm begin-date)
           (mhc-date-dd begin-date)
           (mhc-date-yy end-date)
           (mhc-date-mm end-date)
           (mhc-date-dd end-date)
           (if category  (format " --category=%s" category) "")
           (if search  (format " --search='%s'" search) ""))))

(defun mhc-db-stuck-recurrences ()
  "List stuck recurrences in MHC calendar."
  (mhc-db-flatten
   (mhc-process-send-command
    (format "stuck_recurrences --format=emacs"))))

(defun mhc-db-scan-flat (begin-date end-date &optional nosort category search)
  "Scan MHC database from BEGIN-DATE to END-DATE.
Unlike `mhc-db-scan`, returned value is not grouped by date.
For example:
 ((date . mhc-schedule) (date . mhc-schedule) ...)
If optional NOSORT is non-nil, returned value is not sort.
If optional CATEGORY is non-nil, returned value is clipped by category.
If optional SEARCH is non-nil returned value is clipped by search string."
  (mhc-db-flatten (mhc-db-scan begin-date end-date nosort category search)))

(defun mhc-db-flatten (dayinfo-list)
  "Flatten DAYINFO-LIST scanned from `mhc-db-scan`.
Unlike `mhc-db-scan`, returned value is not grouped by date."
  (apply 'append
         (mapcar (lambda (dayinfo)
                   (let ((date (mhc-day-date dayinfo))
                         (schedules (mhc-day-schedules dayinfo)))
                     (mapcar (lambda (sch) (cons date sch)) schedules)))
                 dayinfo-list)))

(defun mhc-db-search (&rest query)
  (let ((b (mhc-date-new 1970 1 1))
        (e (mhc-date-yy+ (mhc-date-now) 10)))
    (mhc-db-scan b e nil nil (mhc-db/query-to-search-string query))))

(defun mhc-db-past-occurrences (&optional days)
  (let ((now (mhc-date-now)))
    (nreverse
     (mhc-db-scan-flat (mhc-date- now (or days 732)) now))))

(defun mhc-db-make-completion-list (occurrences)
  (delq nil
        (mapcar
         (lambda (occurrence)
           (let ((sch (cdr occurrence)))
             (if  (not (string= (mhc-record-name (mhc-schedule-record sch)) ""))
                 (cons (mhc-db-real-to-display occurrence) occurrence))))
         occurrences)))

(defun mhc-db-real-to-display (occurrence)
  (let* ((date (car occurrence))
         (sch (cdr occurrence))
         (location (mhc-schedule-location sch)))
    (concat
     (mhc-date-format date "%04d/%02d/%02d" yy mm dd)
     (format " %11s " (mhc-schedule-time-as-string sch))
     (mhc-schedule-subject sch)
     (if (and location (not (string= location "")))
         (format " [%s]" location)))))

(defun mhc-db/quote-string (string)
  (format "\"%s\"" string))

(defun mhc-db/keyword-to-string (keyword)
  (format "%s" keyword))

(defun mhc-db/query-to-search-string (query)
  (let ((keywords '(:subject :body :category :recurrence_tag :location)) string)
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

(defun mhc-db-record-path-from-buffer (buffer)
  "Return file path in MHC spool bound to BUFFER.
File path is taken from X-SC-Record-Id field."
  (with-current-buffer buffer
    (let ((spool-directory (file-name-as-directory
                            (expand-file-name
                             "spool" (mhc-config-base-directory))))
          (record-id (mhc-draft-record-id)))
      (expand-file-name (concat record-id ".mhc") spool-directory))))

(defun mhc-db-add-record-from-buffer (buffer &optional allow-overwrite)
  "Add current mhc-draft BUFFER to MHC db.
If optional ALLOW-OVERWRITE is non-nil, do not ask overwrite."
  (let* ((path (mhc-db-record-path-from-buffer buffer))
         (directory (file-name-directory path))
         (overwriting (file-exists-p path)))
    (if (or (not overwriting)
            allow-overwrite
            (y-or-n-p (format "Overwrite existing %s? " path)))
        (with-current-buffer buffer
          (mhc-draft-increment-sequence)
          (mhc-draft-translate)
          (mhc-file-make-directory directory)
          (mhc-write-region-as-coding-system
           mhc-default-coding-system
           (point-min) (point-max) path nil 'nomsg)
          (set-buffer-modified-p nil)
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
        (mhc-db-add-record-from-buffer (current-buffer) t)))))

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

;;; mhc-db.el ends here
