;;; -*- mode: Emacs-Lisp; coding: utf-8 -*-

;; Author:  TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Created: 2000/05/17
;; Revised: $Date$


;;; Commentary:

;; This file is a part of MHC, and includes the simplest Gnus backend
;; for MHC.


;;; Code:

(require 'nnheader)
(require 'nnmail)
(require 'nnoo)
(eval-when-compile (require 'cl))

(gnus-declare-backend "nnmhc" 'physical-address)

(nnoo-declare nnmhc)


;;; Internal Variables:

(defvoo nnmhc-article-list nil)
(defvoo nnmhc-status-string "" nil)

(defvar nnmhc-file-coding-system
  (if (boundp 'MULE) '*noconv* 'raw-text))


;;; Interface functions:

(nnoo-define-basics nnmhc)

(defmacro nnmhc-get-article (num)
  `(car (nth (1- ,num) nnmhc-article-list)))

(defmacro nnmhc-get-subject (num)
  `(cdr (nth (1- ,num) nnmhc-article-list)))

(deffoo nnmhc-retrieve-headers (sequence &optional group server fetch-old)
  (when (integerp (car sequence))
    (save-excursion
      (set-buffer nntp-server-buffer)
      (delete-region (point-min) (point-max))
      (let ((pathname-coding-system 'binary) file begin)
        (dolist (article sequence)
          (when (and
                 (setq file (nnmhc-get-article article))
                 (file-exists-p file)
                 (not (file-directory-p file)))
            (insert (format "221 %d Article retrieved.\n" article))
            (setq begin (point))
            (nnheader-insert-head file)
            (goto-char begin)
            (if (search-forward "\n\n" nil t)
                (forward-char -1)
              (goto-char (point-max))
              (insert "\n\n"))
            (insert ".\n")
            (delete-region (point) (point-max))))
        (nnheader-fold-continuation-lines)
        'headers))))

(deffoo nnmhc-request-close ()
  t)

(deffoo nnmhc-request-article (id &optional group server buffer)
  (let ((nntp-server-buffer (or buffer nntp-server-buffer))
        (pathname-coding-system 'binary)
        path)
    (when (integerp id)
      (setq path (nnmhc-get-article id))
      (cond
       ((not path)
        (nnheader-report 'nnmhc "No such article: %s" id))
       ((not (file-exists-p path))
        (nnheader-report 'nnmhc "No such file: %s" path))
       ((file-directory-p path)
        (nnheader-report 'nnmhc "File is a directory: %s" path))
       ((not (save-excursion
               (let ((nnmail-file-coding-system
                      nnmhc-file-coding-system))
                 (nnmail-find-file path))))
        (nnheader-report 'nnmhc "Couldn't read file: %s" path))
       (t
        (save-excursion
          (set-buffer nntp-server-buffer)
          (goto-char (mhc-header-narrowing
                       (unless (mhc-header-get-value "subject")
                         (insert "Subject: " (nnmhc-get-subject id) "\n"))
                       (mhc-header-delete-header "xref")
                       (insert (format "Xref: %s %s\n" (system-name) path))
                       (point-max)))
          ;; Hack for (gnus-bbdb/update-record), which doesn't accept
          ;; an article consisting of only headers.
          (if (eobp) (insert "\n")))
        (nnheader-report 'nnmhc "Article %s retrieved" id)
        (cons group id))))))

(deffoo nnmhc-request-group (group &optional server fast)
  (nnheader-report 'nnmhc "Selected group %s" group)
  (nnheader-insert "211 1 1 1 %s\n" group))

(deffoo nnmhc-close-group (group &optional server)
  t)


(provide 'nnmhc)

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

;;; nnmhc.el ends here.
