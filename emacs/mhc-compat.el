;;; -*- mode: Emacs-Lisp; coding: utf-8 -*-

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Created: 2000/05/01
;; Revised: $Date$


;;; Commentary:

;; This file is a part of MHC, and includes definitions to absorb
;; incompatibilities between emacsen.


;;; Code:

(if (fboundp 'insert-file-contents-as-coding-system)
    (defalias 'mhc-insert-file-contents-as-coding-system
      'insert-file-contents-as-coding-system)
  (defun mhc-insert-file-contents-as-coding-system
    (coding-system filename &optional visit beg end replace)
    "Like `insert-file-contents', q.v., but CODING-SYSTEM the first arg will
be applied to `coding-system-for-read'."
    (let ((coding-system-for-read coding-system)
          (file-coding-system-for-read coding-system))
      (insert-file-contents filename visit beg end replace))))


(if (fboundp 'write-region-as-coding-system)
    (defalias 'mhc-write-region-as-coding-system
      'write-region-as-coding-system)
  (defun mhc-write-region-as-coding-system
    (coding-system start end filename &optional append visit lockname)
    "Like `write-region', q.v., but CODING-SYSTEM the first arg will be
applied to `coding-system-for-write'."
    (let ((coding-system-for-write coding-system)
          (file-coding-system coding-system))
      (write-region start end filename append visit))))

(if (and (fboundp 'regexp-opt)
         (not (featurep 'xemacs)))
    (defalias 'mhc-regexp-opt 'regexp-opt)
  (defun mhc-regexp-opt (strings &optional paren)
    "Return a regexp to match a string in STRINGS.
Each string should be unique in STRINGS and should not contain any regexps,
quoted or not.  If optional PAREN is non-nil, ensure that the returned regexp
is enclosed by at least one regexp grouping construct."
    (let ((open-paren (if paren "\\(" "")) (close-paren (if paren "\\)" "")))
      (concat open-paren (mapconcat 'regexp-quote strings "\\|") close-paren))))


(if (fboundp 'string-to-char-list)
    (defalias 'mhc-string-to-char-list 'string-to-char-list)
  (defun mhc-string-to-char-list (string)
    (string-to-list string)))

(provide 'mhc-compat)

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

;;; mhc-compat.el ends here
