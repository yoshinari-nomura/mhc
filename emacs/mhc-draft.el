;;; mhc-draft.el --- Draft modules for MHC.

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;;          Yuuichi Teranishi <teranisi@quickhack.net>
;; Created: 2000/07/25
;; Revised: $Date: 2008/07/04 06:01:20 $

;;; Commentary:

;; This file is a part of MHC, includes functions for draft.

;;; Code:

(require 'mhc-calendar)
(require 'mhc-header)

;; Global Variable:

(defconst mhc-draft-buffer-name "*mhc draft*")

(defcustom mhc-draft-import-header-list
  '("Subject:" "From:" "To:" "Cc:" "Date:" "Message-Id:"
    "X-GM-THRID:" "X-GM-MSGID:" "X-GM-LABELS:")
  "*These headers are used when article is imported."
  :group 'mhc
  :type '(repeat string))

(defcustom mhc-draft-mode-hook nil
  "*Hook run in mhc draft mode buffers."
  :group 'mhc
  :type 'hook)

;; Avoid warning of byte-compiler.
(defvar mhc-draft-buffer-file-name nil)

(defvar mhc-draft-mode-map)

(defun mhc-draft-setup-new ()
  "Setup new draft (Insert header separator, etc)."
  (let ((sep-regexp (format "\n\\(%s\\)?\n" (regexp-quote mail-header-separator)))
        (sep (concat "\n" mail-header-separator "\n")))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward sep-regexp nil t)
          (replace-match sep)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert mail-header-separator "\n")))))

(defun mhc-draft-new (&optional template preset-fields)
  "Prepare new mhc-draft buffer.
If TEMPLATE is a string or buffer, it is used for a new draft.
If PRESET-FIELDS is a list of cons-cell like: ((header-name . value) ...),
these fields are set to the draft after import TEMPLATE."
  (interactive)
  (let ((draft-buffer (generate-new-buffer mhc-draft-buffer-name)))
        (with-current-buffer draft-buffer
          ;; insert template
          (cond
           ((bufferp template)
            (insert-buffer-substring-no-properties template))
           ((stringp template)
            (insert template)))
          ;; insert header separator
          (mhc-draft-setup-new)
          (mhc-draft-delete-garbage-headers)
          (mhc-draft-setup-headers preset-fields)
          (mhc-draft-mode)
        (switch-to-buffer draft-buffer t)
        (goto-char (point-min)))))

(defvar mhc-draft-template)

(defun mhc-draft-store-template (template)
  "Store common draft template to TEMPLATE."
  (setq mhc-draft-template template))

(defun mhc-draft-template ()
  "Get common draft template."
  mhc-draft-template)

(defsubst mhc-draft-reedit-buffer (buffer &optional original)
  "Restore contents of BUFFER as draft in the current buffer.
If optional argument ORIGINAL is non-nil, BUFFER is raw buffer."
  (unless (eq (current-buffer) buffer)
    (erase-buffer)
    (insert-buffer-substring buffer))
  (mhc-header-narrowing
    (mhc-header-delete-header
     "^\\(Content-.*\\|Mime-Version\\|User-Agent\\):" 'regexp))
  (mhc-header-decode-ewords)
  (goto-char (point-min))
  (when (re-search-forward "^\r?$" nil t)
    (insert mail-header-separator)))

(defsubst mhc-draft-reedit-file (filename)
  "Restore contents of file FILENAME as draft in the current buffer."
  (erase-buffer)
  (mhc-insert-file-contents-as-coding-system
   mhc-default-coding-system filename)
  (mhc-draft-reedit-buffer (current-buffer) 'original))


(defsubst mhc-draft-translate ()
  "Convert an article in the current buffer to an ENCODED one.
ENCODED article should be valid for storeing to a mhc file."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward (concat "\n" mail-header-separator "\n") nil t)
      (replace-match "\n\n"))))


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

(defun mhc-draft-increment-sequence ()
  "Increment X-SC-Sequence in mhc-draft buffer."
  (mhc-header-narrowing
    (let ((sequence (or (mhc-header-get-value "x-sc-sequence") "0")))
      (mhc-header-put-value "x-sc-sequence"
                            (1+ (string-to-number sequence))))))

(defun mhc-draft-remove-tailers ()
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward
         (regexp-quote mhc-message-end-of-messge-marker)
         (- (point) (length mhc-message-end-of-messge-marker)) t)
        (replace-match ""))
    (unless (bolp) (insert "\n"))))

(defun mhc-draft-delete-garbage-headers ()
  (mhc-header-narrowing
    (mhc-header-delete-header
     (concat "^\\(" (mhc-regexp-opt mhc-draft-import-header-list) "\\)")
     'regexp)))

(defun mhc-draft-setup-headers (&optional headers-values)
  "Put X-SC-* headers to draft.
HEADERS-VALUES is a list of cons-cell like: ((header-name . value) ...)."
  (let ((xsc-headers (mapcar (lambda (v) (downcase (substring v 0 -1)))
                             (mhc-header-list)))
        (item))
    (mhc-header-narrowing
      (mapc
       (lambda (xsc)
         (if (setq item (assoc xsc headers-values))
             (mhc-header-put-value xsc (or (cdr item) ""))
           (unless (mhc-header-get-value xsc)
             (mhc-header-put-value xsc ""))))
       xsc-headers))))

(defun mhc-draft-record-id ()
  "Get X-SC-Record-Id header value in draft buffer."
  (mhc-header-narrowing
    (mhc-header-get-value "x-sc-record-id")))

(defun mhc-draft-validate-buffer (&optional buffer)
  "Validate mhc draft BUFFER.
If BUFFER is omitted, current buffer will be validated."
  (interactive)
  (let ((validation (mhc-process-send-command-with-buffer
                     "validate --format=emacs"
                     (or buffer (current-buffer)))))
    (if (and (stringp validation)
             (string-match "^OK" validation))
        (message "Validation passed.")
      ;; \\' means end of string (not end of each line)
      (error "ERROR: %s" (replace-regexp-in-string "[.\r\n]+\\'" "" validation)))))

(defun mhc-draft-finish ()
  "Add current draft as a schedule."
  (interactive)
  (mhc-draft-validate-buffer)
  (mhc-calendar-input-exit)
  (if (mhc-db-add-record-from-buffer
       (current-buffer)
       (not (called-interactively-p 'interactive)))
      (progn
        (kill-buffer (current-buffer))
        (mhc-window-pop)
        (or (and (mhc-summary-buffer-p)
                 (mhc-rescan-month mhc-default-hide-private-schedules))
            (and (mhc-calendar-p) (mhc-calendar-rescan)))
        (run-hooks 'mhc-draft-finish-hook)
        (message "Successfully registered."))))

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
