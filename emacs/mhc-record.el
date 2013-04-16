;;; -*- mode: Emacs-Lisp; coding: utf-8 -*-

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Created: 2000/05/15
;; Revised: $Date$


;;; Commentary:

;; This file is a part of MHC, and includes functions manipulate
;; MHC-RECORD structure.


;;; About MHC-RECORD structure:

;; Each MHC-RECORD structure is a cons cell has a construction as
;; follows:
;;
;;     MHC-RECORD ::= ( KEY . VALUE )
;;     KEY        ::= string ( represents file name of record )
;;     VALUE      ::= [ ID SCHEDULES SEXP ]
;;     ID         ::= string ( represents unique id of recort )
;;     SCHEDULES  ::= MHC-SCHEDULE*
;;     SEXP       ::= S expression to get schedule.


;;; Code:

(require 'mhc-summary)
(require 'mhc-file)
(require 'mhc-draft)


;; Global Variable:

(defcustom mhc-record-log-file
  (expand-file-name ".mhc-db-log" (mhc-summary-folder-to-path mhc-base-folder))
  "*スケジュールファイルの操作履歴ログ"
  :group 'mhc
  :type 'file)


;; Internal Variable:

(defvar mhc-record/id-counter 0)


;; Functions:

(defun mhc-record-create-id ()
  "Return unique ID string."
  (let ((uid (user-login-name))
        (time (format-time-string "%Y%m%d%H%M%S" (current-time)))
        (sequence (format "%04d" mhc-record/id-counter))
        (host (system-name)))
    (setq mhc-record/id-counter (1+ mhc-record/id-counter))
    (concat "<" time sequence "." uid "@" host ">")))

(defun mhc-record-new (name &optional id schedules sexp)
  "Constructer of MHC-RECORD structure."
  (cons name
        (vector (or id (mhc-record-create-id))
                schedules
                sexp)))

(defmacro mhc-record/key (record)
  `(car ,record))
(defmacro mhc-record/value (record)
  `(cdr ,record))

(defmacro mhc-record-name (record)
  `(mhc-record/key ,record))
(defmacro mhc-record-id (record)
  `(aref (mhc-record/value ,record) 0))
(defmacro mhc-record-schedules (record)
  `(aref (mhc-record/value ,record) 1))
(defmacro mhc-record-sexp (record)
  `(aref (mhc-record/value ,record) 2))

(defmacro mhc-record-set-name (record name)
  `(setcar ,record ,name))
(defmacro mhc-record-set-id (record id)
  `(aset (mhc-record/value ,record) 0 ,id))
(defmacro mhc-record-set-schedules (record schedules)
  `(aset (mhc-record/value ,record) 1 ,schedules))
(defmacro mhc-record-set-sexp (record sexp)
  `(aset (mhc-record/value ,record) 2 ,sexp))

(defun mhc-record-copy (record)
  (cons (copy-sequence (mhc-record/key record))
        (copy-sequence (mhc-record/value record))))

(defun mhc-record-subject (record)
  (catch 'found
    (let ((schedules (mhc-record-schedules record)))
      (while schedules
        (if (mhc-schedule-subject (car schedules))
            (throw 'found (mhc-schedule-subject (car schedules))))
        (setq schedules (cdr schedules))))))

(defun mhc-record-subject-as-string (record)
  (or (mhc-record-subject record)
      "(none)"))

(defun mhc-record-occur-multiple-p (record)
  "Return t if RECORD occurs multiple times."
  (let ((schedules (mhc-record-schedules record)))
    (or (> (length schedules) 1)
        (mhc-logic-occur-multiple-p (mhc-schedule-condition (car schedules))))))

(defun mhc-record-write-buffer (record buffer &optional old-record)
  "Write BUFFER to RECORD."
  (let ((modify (file-exists-p (mhc-record-name record))))
    (save-excursion
      (set-buffer buffer)
      (mhc-draft-translate)
      (mhc-write-region-as-coding-system mhc-default-coding-system
                                         (point-min)
                                         (point-max)
                                         (mhc-record-name record)
                                         nil 'nomsg)
      (set-buffer-modified-p nil)
      (if modify
          (prog1
              (mhc-file-modify (mhc-record-name record))
            (mhc-record/append-log record 'modify))
        (if old-record
            (prog2
                (mhc-file-remove (mhc-record-name old-record))
                (mhc-file-add (mhc-record-name record))
              (mhc-record/append-log record 'modify))
          (prog1
              (mhc-file-add (mhc-record-name record))
            (mhc-record/append-log record 'add)))))))

(defun mhc-record-delete (record)
  (prog1 (mhc-file-remove (mhc-record-name record))
    (mhc-record/append-log record 'delete)))

(defun mhc-record/append-log (record status)
  (if mhc-record-log-file
      (let ((tmp-buffer (mhc-get-buffer-create " *mhc-record-append-log*")))
        (save-excursion
          (set-buffer tmp-buffer)
          (delete-region (point-min) (point-max))
          (insert (format "%c %s %s %s %s\n"
                          (cond
                           ((eq status 'add) ?A)
                           ((eq status 'delete) ?D)
                           ((eq status 'modify) ?M)
                           (t ??))
                          (format-time-string "%Y-%m-%d %T")
                          (mhc-record-id record)
                          (mhc-record-name record)
                          (mhc-record-subject-as-string record)))
          (mhc-write-region-as-coding-system mhc-default-coding-system
                                             (point-min)
                                             (point-max)
                                             mhc-record-log-file
                                             'append
                                             'nomsg)))))



(provide 'mhc-record)

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

;;; mhc-record.el ends here.
