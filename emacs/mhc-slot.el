;;; -*- mode: Emacs-Lisp; coding: utf-8 -*-

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Created: 2000/04/30
;; Revised: $Date$


;;; Commentary:

;; This is a part of MHC.  This file includes functions to manipulate
;; cache of schedule files.


;;; About MHC-SLOT structure.

;; Each MHC-SLOT structure is a cons cell has a construction as
;; follows:
;;
;;     MHC-SLOT ::= ( KEY . VALUE )
;;     KEY      ::= ( YEAR . MONTH )
;;     YEAR     ::= integer
;;     MONTH    ::= integer
;;     VALUE    ::= [ MTIME RECORDS ]
;;     MTIME    ::= integer
;;     RECORDS  ::= MHC-RECORD*


;;; Code:

(require 'mhc-parse)
(require 'mhc-vars)


;; Internal Variables:

(defvar mhc-slot/cache nil)


;; Function and macros to manipulate MHC-SLOT structure:

(defun mhc-slot/new (slotkey &optional mtime records)
  (cons slotkey (vector mtime records)))

(defmacro mhc-slot/key (slotinfo)
  `(car ,slotinfo))
(defmacro mhc-slot/value (slotinfo)
  `(cdr ,slotinfo))

(defmacro mhc-slot-mtime (slotinfo)
  `(aref (mhc-slot/value ,slotinfo) 0))
(defmacro mhc-slot-records (slotinfo)
  `(aref (mhc-slot/value ,slotinfo) 1))

(defmacro mhc-slot/set-mtime (slotinfo mtime)
  `(aset (mhc-slot/value ,slotinfo) 0 ,mtime))
(defmacro mhc-slot/set-records (slotinfo records)
  `(aset (mhc-slot/value ,slotinfo) 1 ,records))


;; Functions to manipulate cache:

(defun mhc-slot-clear-cache ()
  "*Clear all cache."
  (interactive)
  (setq mhc-slot/cache nil))

(defun mhc-slot/cache-live-p (slotinfo)
  (let* ((mtime (mhc-misc-get-mtime
                 (mhc-slot-key-to-directory (mhc-slot/key slotinfo))))
         (cache (mhc-slot-mtime slotinfo))
         (mtime-ms (car mtime))
         (mtime-ls (car (cdr mtime)))
         (cache-ms (car cache))
         (cache-ls (car (cdr cache))))
    (cond
     ((null mtime)
      t)                                ; directory doesn't exist yet.
     ((null cache)
      nil)
     ((< cache-ms mtime-ms)
      nil)
     ((= cache-ms mtime-ms)
      (if (>= cache-ls mtime-ls)
          t                             ; t if same.
        nil))
     (t t))))

(defsubst mhc-slot/check-cache (key)
  (cond
   ;; Access cache without checking mtime.
   ((eq mhc-use-cache 0)
    (assoc key mhc-slot/cache))
   ;; Access cache with checking mtime.
   (mhc-use-cache
    (let ((slotinfo (assoc key mhc-slot/cache)))
      (if slotinfo
          (if (mhc-slot/cache-live-p slotinfo)
              slotinfo
            (setq mhc-slot/cache (delq slotinfo mhc-slot/cache))
            nil))))))

(defsubst mhc-slot/set-current-mtime (slotinfo)
  (or (eq mhc-use-cache 0)
      (mhc-slot/set-mtime
       slotinfo
       (mhc-misc-get-mtime (mhc-slot-key-to-directory (mhc-slot/key slotinfo))))))

(defsubst mhc-slot/store-cache (slotinfo)
  (if mhc-use-cache
      (progn
        (mhc-slot/set-current-mtime slotinfo)
        (setq mhc-slot/cache
              (cons slotinfo
                    (let ((x (assoc (mhc-slot/key slotinfo) mhc-slot/cache)))
                      (if x (delq x mhc-slot/cache) mhc-slot/cache))))))
  slotinfo)

(defun mhc-slot-destruct-cache (directory)
  "Destruct cache of schedule files on DIRECTORY."
  (let ((cache (assoc (mhc-slot-directory-to-key directory) mhc-slot/cache)))
    (setq mhc-slot/cache (delq cache mhc-slot/cache))))

(defun mhc-slot-update-cache (key operation record)
  (cond
   ((eq operation 'add) (mhc-slot/add-file key record))
   ((eq operation 'remove) (mhc-slot/remove-file key record))
   (t (error "Internal ERROR: not defined operation(%s)." operation))))

(defun mhc-slot/add-file (key record)
  (let (slot x)
    (if (setq slot (mhc-slot/check-cache key))
        (progn
          (mhc-slot/set-records
           slot
           (cons record
                 (if (setq x (assoc (mhc-record-name record) (mhc-slot-records slot)))
                     (delq x (mhc-slot-records slot))
                   (mhc-slot-records slot))))
          (mhc-slot/set-current-mtime slot)))))

(defun mhc-slot/remove-file (key record)
  (let (slot x)
    (if (setq slot (mhc-slot/check-cache key))
        (progn
          (mhc-slot/set-records
           slot
           (if (setq x (assoc (mhc-record-name record) (mhc-slot-records slot)))
               (delq x (mhc-slot-records slot))
             (message "Internal Warning: there is no information about specified file in cache.")
             (mhc-slot-records slot)))
          (mhc-slot/set-current-mtime slot)))))


;; Functions to manipulate slot key:

(defsubst mhc-slot-key-to-directory (key) "\
指定された KEY ::= (YEAR . MONTH) に対応する適当なディレクトリを返す
ただし (nil . nil) が指定された場合は intersect/ を返す"
  (file-name-as-directory
   (expand-file-name (if (equal key '(nil . nil))
                         "intersect"
                       (format "%04d/%02d" (car key) (cdr key)))
                     (mhc-summary-folder-to-path mhc-base-folder))))

(defsubst mhc-slot-directory-to-key (directory)
  "mhc-slot-month-to-directory の逆関数"
  (setq directory (expand-file-name directory))
  (let ((base (regexp-quote (mhc-summary-folder-to-path mhc-base-folder))))
    (cond
     ((string-match (concat "^" base "/intersect/?$")
                    directory)
      (cons nil nil))
     ((string-match (concat "^" base "/\\([0-9][0-9][0-9][0-9]\\)/\\(0[1-9]\\|1[012]\\)/?$")
                    directory)
      (cons (string-to-number (match-string 1 directory))
            (string-to-number (match-string 2 directory))))
     (t
      (error "Illegal argument: directory=%s" directory)))))


;; Interface functions:

(defun mhc-slot-get-month-schedule (key)
  (or (mhc-slot/check-cache key)
      (let* ((slotinfo (mhc-slot/new key))
             (directory (mhc-slot-key-to-directory key))
             (entries (if (file-directory-p directory)
                          (directory-files directory nil nil t)))
             records filename)
        (while entries
          (and (not (string-match "[^0-9]" (car entries)))
               (file-regular-p (setq filename (expand-file-name (car entries) directory)))
               (setq records (cons (mhc-parse-file filename)
                                   records)))
          (setq entries (cdr entries)))
        (mhc-slot/set-records slotinfo records)
        (mhc-slot/store-cache slotinfo))))

(defun mhc-slot-get-constant-schedule ()
  (let ((mhc-use-cache 0))
    (or (mhc-slot/check-cache (cons nil 'constant-schedule))
        (if (file-readable-p mhc-schedule-file)
            (let ((slotinfo (mhc-slot/new (cons nil 'constant-schedule)))
                  records)
              (save-excursion
                (set-buffer (mhc-get-buffer-create " *mhc-parse-file*"))
                (delete-region (point-min) (point-max))
                (mhc-insert-file-contents-as-coding-system
                 mhc-default-coding-system
                 (expand-file-name mhc-schedule-file))
                (goto-char (point-min))
                (while (not (eobp))
                  (if (eq (following-char) ?#)
                      (delete-region (point) (progn (forward-line 1) (point)))
                    (forward-line 1)))
                (goto-char (point-min))
                (while (progn (skip-chars-forward " \t\n") (not (eobp)))
                  (delete-region (point-min) (point))
                  (mhc-header-narrowing
                    (setq records
                          (cons (mhc-parse-buffer) records))
                    (delete-region (point-min) (point-max)))))
              (mhc-slot/set-records slotinfo (nreverse records))
              (mhc-slot/store-cache slotinfo))))))

(defun mhc-slot-get-intersect-schedule ()
  (mhc-slot-get-month-schedule '(nil . nil)))



(provide 'mhc-slot)

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

;;; mhc-slot.el ends here.
