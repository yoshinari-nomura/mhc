;;; mhc-mua.el --- MUA backend for MHC

;; Author:  Yoshinari Nomura <nom@quickhack.net>
;; Created: 2014-09-20

;;; Commentary:

;; This file is a part of MHC, backend methods for Generic MUA.

;;; Code:

;; Setup function:

;;;###autoload
(defun mhc-mua-setup ()
  (require 'mhc)
  (setq mhc-mailer-package 'mua)
  (mhc-setup)
  (add-hook 'mhc-summary-mode-hook 'mhc-mode))

;;; Backend methods:

;;; for mhc-summary

(defvar mhc-message-eof-marker-face 'mhc-message-eof-marker-face)
(defvar mhc-message-subject-face 'mhc-message-subject-face)

(defface mhc-message-eof-marker-face
  '((((class color)
      (background dark))
     (:background "aquamarine2"))
    (((class color)
      (background light))
     (:background "aquamarine2"))
    (t
     ()))
  "*Face used by mhc-message-eof-marker."
  :group 'mhc-faces)

(defface mhc-message-subject-face
  '((((class color)
      (background dark))
     (:foreground "OrangeRed" :bold t))
    (((class color)
      (background light))
     (:foreground "Firebrick" :bold t))
    (t
     ()))
  "*Face used by mhc-message-subject."
  :group 'mhc-faces)

(defvar mhc-message-font-lock-keywords
  '(("\\([12][0-9][0-9][0-9]\\)\\([0-1][0-9]\\)\\([0-3][0-9]\\)"
     (1 font-lock-type-face)
     (2 font-lock-comment-face)
     (3 font-lock-builtin-face))
    ("\\(X-SC-\\(Subject\\|Location\\|Day\\|Time\\|Category\\|Priority\\|Recurrence-Tag\\|Mission-Tag:\\|Cond\\|Duration\\|Alarm\\|Record-Id\\|Sequence\\):\\)"
     (1 font-lock-keyword-face))
    ("\\(\\[End of message\\]\\)"
     (1 mhc-message-eof-marker-face))
    ("\\(X-SC-Subject:\\) *\\(.*\\)"
     (1 font-lock-keyword-face)
     (2 mhc-message-subject-face))
    ))

(defun mhc-mua-highlight-message (&optional for-draft)
  "Hilight message in the current buffer.
If FOR-DRAFT is non-nil, Hilight message as draft message."
  (set (make-local-variable 'font-lock-defaults)
       '(mhc-message-font-lock-keywords t)))

;;; for mhc-calendar

(provide 'mhc-mua)
(put 'mhc-mua 'highlight-message       'mhc-mua-highlight-message)
(put 'mhc-mua 'summary-search-date     'mhc-mua-summary-search-date)

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

;;; mhc-mua.el ends here
