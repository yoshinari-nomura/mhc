;;; mhc-draft.el --- Draft modules for MHC.

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;;          Yuuichi Teranishi <teranisi@quickhack.net>
;; Created: 2000/07/25
;; Revised: $Date: 2008/07/04 06:01:20 $

;;; Commentary:

;; This file is a part of MHC, includes functions for draft.

;;; About MUA Backend:

;; In order to define new MUA backend, it is required to define these
;; methods.
;;
;;     (mhc-foo-draft-setup-new)
;;         Setup new draft (Insert header separator).
;;
;;     (mhc-foo-draft-reedit-buffer BUFFER ORIGINAL)
;;         Restore content of BUFFER as draft in the current buffer.
;;         If ORIGINAL is non-nil, use BUFFER as raw buffer.
;;
;;     (mhc-foo-draft-reedit-file FILENAME)
;;         Restore contents of file FILENAME as draft in the current buffer.
;;
;;     (mhc-foo-draft-translate)
;;         Translate current buffer to raw buffer.
;;
;; Define these methods appropriately, and put definitions as follows:
;;
;;    (put 'mhc-foo 'draft-setup-new 'mhc-foo-draft-setup-new)
;;    (put 'mhc-foo 'draft-reedit-buffer 'mhc-foo-draft-reedit-buffer)
;;    (put 'mhc-foo 'draft-reedit-file 'mhc-foo-draft-reedit-file)
;;    (put 'mhc-foo 'draft-translate 'mhc-foo-draft-translate)

;;; Code:

(require 'mhc-summary)

;; Global Variable:

(defconst mhc-draft-buffer-name "*mhc draft*")

(defcustom mhc-draft-unuse-hdr-list
  '(">From " "From " "Delivered-To:" "Delivery-date:" "Envelope-to:"
    "Errors-To:" "Gnus-Warning:" "Lines:" "Posted:" "Precedence:" "Received:"
    "Replied:" "Return-Path:" "Sender:" "User-Agent:" "X-Bogosity:"
    "X-Dispatcher:" "X-Filter:" "X-Gnus-Mail-Source:" "X-Mailer:" "X-Received:"
    "X-Sender:" "X-Seqno:" "X-Spam-Flag:" "X-Spam-Probability:" "X-UIDL:"
    "Xref:")
  "*These headers are removed when article is imported."
  :group 'mhc
  :type '(repeat string))

(defcustom mhc-draft-mode-hook nil
  "*Hook run in mhc draft mode buffers."
  :group 'mhc
  :type 'hook)

;; Avoid warning of byte-compiler.
(defvar mhc-draft-buffer-file-name nil)

(defvar mhc-draft-mode-map)

(defsubst mhc-draft-setup-new ()
  "Setup new draft (Insert header separator, etc)."
  (funcall (mhc-get-function 'draft-setup-new)))


(defsubst mhc-draft-reedit-buffer (buffer &optional original)
  "Restore contents of BUFFER as draft in the current buffer.
If optional argument ORIGINAL is non-nil, BUFFER is raw buffer."
  (funcall (mhc-get-function 'draft-reedit-buffer) buffer original))


(defsubst mhc-draft-reedit-file (filename)
  "Restore contents of file FILENAME as draft in the current buffer."
  (funcall (mhc-get-function 'draft-reedit-file) filename))


(defsubst mhc-draft-translate ()
  "Translate current buffer to raw buffer."
  (funcall (mhc-get-function 'draft-translate)))


(define-derived-mode mhc-draft-mode
  text-mode
  "MHC-Draft"
  "Major mode for editing schdule files of MHC.
Like Text Mode but with these additional commands:
C-c C-c  mhc-draft-finish
C-c C-k  mhc-draft-kill
C-c C-q  mhc-draft-kill
C-c ?    mhc-draft-insert-calendar
.
"
  (define-key mhc-draft-mode-map "\C-c\C-c" 'mhc-draft-finish)
  (define-key mhc-draft-mode-map "\C-c\C-q" 'mhc-draft-kill)
  (define-key mhc-draft-mode-map "\C-c\C-k" 'mhc-draft-kill)
  (define-key mhc-draft-mode-map "\C-c?" 'mhc-draft-insert-calendar)
  (make-local-variable 'adaptive-fill-regexp)
  (setq adaptive-fill-regexp
        (concat "[ \t]*[-a-z0-9A-Z]*\\(>[ \t]*\\)+[ \t]*\\|"
                adaptive-fill-regexp))
  (unless (boundp 'adaptive-fill-first-line-regexp)
    (setq adaptive-fill-first-line-regexp nil))
  (make-local-variable 'adaptive-fill-first-line-regexp)
  (setq adaptive-fill-first-line-regexp
        (concat "[ \t]*[-a-z0-9A-Z]*\\(>[ \t]*\\)+[ \t]*\\|"
                adaptive-fill-first-line-regexp))
  (mhc-highlight-message t)
  (set (make-local-variable 'indent-tabs-mode) nil))

(defun mhc-draft-kill (&optional no-confirm)
  "Kill current draft.
If optional argument NO-CONFIRM is non-nil, kill without confirmation."
  (interactive "P")
  (if (or no-confirm (y-or-n-p "Kill draft buffer? "))
      (progn
        (message "")
        (mhc-calendar-input-exit)
        (kill-buffer (current-buffer))
        (mhc-window-pop))))

(defvar mhc-draft-finish-hook nil
  "Hook run after `mhc-draft-finish'.")

(defun mhc-draft-set-as-done ()
  "Set current draft as DONE."
  (interactive)
  (if (mhc-draft-in-category-p "todo")
      (mhc-draft-append-category "Done")))

(defun mhc-draft-set-as-not-done ()
  "Set current draft as NOT-DONE."
  (interactive)
  (if (mhc-draft-in-category-p "todo")
      (mhc-draft-delete-category "done")))

(defun mhc-draft-toggle-done ()
  "Set current draft as DONE if not; remove done if there."
  (interactive)
  (if  (mhc-draft-in-category-p "todo")
      (if (mhc-draft-in-category-p "done")
          (mhc-draft-delete-category "done")
        (mhc-draft-append-category "Done"))))

(defun mhc-draft-append-category (category)
  "Append CATEGORY if it is not contained yet."
  (mhc-header-narrowing
    (let ((categories (mhc-header-get-value "x-sc-category")))
      (unless (string-match category categories)
        (mhc-header-put-value "x-sc-category"
                              (concat categories " " category))))))

(defun mhc-draft-in-category-p (category)
  (mhc-header-narrowing
    (string-match (concat "[ \t]*" category)
                  (mhc-header-get-value "x-sc-category"))))

(defun mhc-draft-delete-category (category)
  "Delete CATEGORY if it is contained."
  (mhc-header-narrowing
    (let ((categories (mhc-header-get-value "x-sc-category")))
      (when (string-match (concat "[ \t]*" category) categories)
        (setq categories (concat
                          (substring categories 0 (match-beginning 0))
                          (substring categories (match-end 0))))
        (when (string-match "[ \t]+$" categories)
          (setq categories (substring categories 0 (match-beginning 0))))
        (mhc-header-put-value "X-SC-Category" categories)))))

(defun mhc-draft-finish ()
  "Add current draft as a schedule."
  (interactive)
  (let ((record
         (mhc-parse-buffer (mhc-record-new mhc-draft-buffer-file-name)
                           'strict)))
    (mhc-calendar-input-exit)
    (if (mhc-db-add-record-from-buffer record (current-buffer)
                                       (not (interactive-p)))
        (progn
          (kill-buffer (current-buffer))
          (mhc-window-pop)
          (or (and (mhc-summary-buffer-p)
                   (mhc-rescan-month mhc-default-hide-private-schedules))
              (and (mhc-calendar-p) (mhc-calendar-rescan)))
          (run-hooks 'mhc-draft-finish-hook)))))

(provide 'mhc-draft)

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

;;; mhc-draft.el ends here
