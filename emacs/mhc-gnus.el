;;; -*- mode: Emacs-Lisp; coding: utf-8 -*-

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;;          MIYOSHI Masanori <miyoshi@hrl.hitachi.co.jp>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Created: 2000/05/10
;; Revised: $Date$


;;; Commentary:

;; This file is a part of MHC, and includes MUA backend methods for
;; Gnus.

;; When you use T-gnus, you should add the following expression to
;; your ~/.emacs.
;;
;;     (add-hook 'mhc-draft-mode-hook 'mime-edit-mode)
;;
;; Otherwise, you should add the following expression to your
;; ~/.emacs.
;;
;;     (add-hook 'mhc-draft-mode-hook 'mml-mode)

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'gnus-art))
(require 'gnus-sum)
(require 'nnmhc)

;; To suppress byte-compile warnings.
(eval-when-compile
  (defvar gnus-original-article-buffer))
(eval-and-compile
  (autoload 'eword-encode-string "eword-encode")
  (autoload 'gnus-backlog-remove-article "gnus-bcklg")
  (autoload 'gnus-copy-article-buffer "gnus-msg")
  (autoload 'mime-to-mml "mml")
  (autoload 'rfc2047-decode-region "rfc2047")
  (autoload 'rfc2047-decode-string "rfc2047")
  (autoload 'rfc2047-encode-message-header "rfc2047")
  (autoload 'rfc2047-encode-string "rfc2047"))


;;; Internal Variables:

(defvar mhc-gnus/mhc-is-running nil)


;;; Setup function:

;;;###autoload
(defun mhc-gnus-setup ()
  (require 'mhc)
  (setq mhc-mailer-package 'gnus)
  (mhc-gnus/setup-methods)
  (mhc-file-toggle-offline nil (not gnus-plugged) nil)
  (mhc-setup)
  (add-hook 'gnus-group-mode-hook 'mhc-mode)
  (add-hook 'gnus-summary-mode-hook 'mhc-mode)
  (add-hook 'gnus-exit-gnus-hook 'mhc-exit)
  (add-hook 'gnus-agent-plugged-hook 'mhc-gnus/plugged)
  (add-hook 'gnus-agent-unplugged-hook 'mhc-gnus/unplugged))

(defun mhc-gnus/plugged ()
  (mhc-file-toggle-offline nil nil))

(defun mhc-gnus/unplugged ()
  (mhc-file-toggle-offline nil t))

;;; Backend methods:

;;; Summary APIs (mhc-summary.el):
(defun mhc-gnus-summary-filename ()
  "Return the file name of the article on the current line."
  (let ((num (get-text-property (point) 'gnus-number)))
    (if num (nnmhc-get-article num))))

(defun mhc-gnus-summary-display-article ()
  "Display the article on the current line."
  (let ((num (get-text-property (point) 'gnus-number)))
    (if num (gnus-summary-display-article num))))

(defun mhc-gnus-get-import-buffer (get-original)
  "Return a buffer visiting import article.
If GET-ORIGINAL is non-nil, return a pair of buffer: one keeps a MIME
message and the other keeps a visible message.

NOTE: This function designed for original Gnus, not for T-gnus.  When
using T-gnus, `mhc-mime-get-import-buffer' must be used instead of
this function."
  (gnus-summary-select-article)
  (if get-original
      (cons gnus-original-article-buffer (gnus-copy-article-buffer))
    (gnus-copy-article-buffer)))

(defsubst mhc-gnus/date-to-group-name (date)
  (mhc-date-format date "%s/%02d/%02d" mhc-base-folder yy mm))

(defun mhc-gnus-generate-summary-buffer (date)
  "Generate a summary buffer for DATE, and change current buffer to it."
  (let* ((group (mhc-gnus/date-to-group-name date))
         (method `(nnmhc ,group))
         (vgroup (gnus-group-prefixed-name group method)))
    ;; initialize ephemeral nnmhc group.
    (gnus-group-read-ephemeral-group vgroup method t
                                     (if (buffer-live-p gnus-summary-buffer)
                                         (cons gnus-summary-buffer 'summary)
                                       (cons (current-buffer) 'group))
                                     t)
    (gnus-group-read-group 0 t vgroup)
    (gnus-summary-make-local-variables)
    (setq inhibit-read-only t
          nnmhc-article-list nil)
    (delete-region (point-min) (point-max))))

;; This is a trick to suppress byte-compile of the inline function
;; `make-full-mail-header' defined by `defsubst'.  Cf. (ELF:01937)
(defalias 'mhc-gnus/make-full-mail-header 'make-full-mail-header)
(defalias 'mhc-gnus/encode-string 'identity)

(defun mhc-gnus-insert-summary-contents (inserter)
  "Insert `mhc-tmp-schedule' with INSERTER."
  (let ((x (mhc-record-name (mhc-schedule-record mhc-tmp-schedule)))
        (subject (mhc-gnus/encode-string
                  (mhc-schedule-subject-as-string mhc-tmp-schedule)))
        (pos (point)))
    (when x
      (push (cons x subject) nnmhc-article-list)
      (setq x (length nnmhc-article-list)))
    (funcall inserter)
    (if x
        (let ((header (mhc-gnus/make-full-mail-header x subject "")))
          (put-text-property pos (point) 'gnus-number x)
          (push (gnus-data-make x 0 0 header 0) gnus-newsgroup-data))
      (remove-text-properties pos (point) '(gnus-number nil)))
    (insert "\n")))

(defun mhc-gnus-summary-mode-setup (date)
  "Setup this current buffer as a summary buffer for DATE."
  (setq gnus-newsgroup-data (nreverse gnus-newsgroup-data)
        nnmhc-article-list (nreverse nnmhc-article-list))
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((num (get-text-property (point) 'gnus-number)))
        (if num (gnus-data-set-pos (assoc num gnus-newsgroup-data) (point))))
      (forward-line 1)))
  (let ((group (gnus-group-prefixed-name
                (mhc-gnus/date-to-group-name date) '(nnmhc))))
    ;; Reset all caches for this group.
    (let ((i 0))
      (while (<= (incf i) (length nnmhc-article-list))
        (gnus-backlog-remove-article group i)))
    ;; Reset an article kept in `gnus-original-article-buffer'.
    (when (gnus-buffer-live-p gnus-original-article-buffer)
      (with-current-buffer gnus-original-article-buffer
        (setq gnus-original-article nil)))
    (let ((gnus-newsgroup-data))
      (gnus-summary-mode group)))
  (when (fboundp 'gnus-summary-setup-default-charset)
    (gnus-summary-setup-default-charset)) ; for Nana7
  (set (make-local-variable 'mhc-gnus/mhc-is-running) t)
  (set (make-local-variable 'gnus-visual) nil)
  (set (make-local-variable 'gnus-auto-extend-newsgroup) nil)
  (setq gnus-article-current nil ; Reset structures of the current article.
        gnus-current-article nil
        gnus-current-headers nil
        gnus-newsgroup-begin 1
        gnus-newsgroup-end (length nnmhc-article-list)))

(defun mhc-gnus-highlight-message (for-draft)
  "Hilight message in the current buffer.
If FOR-DRAFT is non-nil, Hilight message as draft message."
  (if for-draft
      (progn
        (require 'message)
        (set (make-local-variable 'font-lock-defaults)
             '(message-font-lock-keywords t)))
    (let ((gnus-article-buffer (current-buffer))
          ;; Adhoc fix to avoid errors in gnus-article-add-buttons().
          (gnus-button-marker-list))
      (gnus-article-highlight))))

(defalias 'mhc-gnus-decode-string 'rfc2047-decode-string)

(defun mhc-gnus/decode-buffer ()
  (goto-char (point-min))
  (skip-chars-forward "[ \t\r\f\n\x20-\x7e]")
  (unless (eobp)
    (decode-coding-region (point-min) (point-max) mhc-default-coding-system)))

(defun mhc-gnus-decode-header ()
  "Decode MIME-encoded headers.

NOTE: This function designed for original Gnus, not for T-gnus.  When
using T-gnus, `mhc-mime-decode-header' must be used instead of this
function."
  (save-restriction
    (mail-narrow-to-head)
    (mhc-gnus/decode-buffer)
    (rfc2047-decode-region (point-min) (point-max))))


;;; Draft APIs (mhc-draft.el):
(defun mhc-gnus-draft-setup-new ()
  "Setup new draft (Insert header separator).

NOTE: This function designed for original Gnus, not for T-gnus.  When
using T-gnus, `mhc-mime-draft-setup-new' must be used instead of this
function."
  (goto-char (point-min))
  (insert mail-header-separator "\n"))

(defun mhc-gnus-draft-reedit-buffer (buffer original)
  "Prepare a draft from the content of the BUFFER.
If ORIGINAL is non-nil, this function converts a MIME message in the
BUFFER into a human-editable text written in MML, a language for
describing MIME parts.

NOTE: This function designed for original Gnus, not for T-gnus.  When
using T-gnus, `mhc-mime-draft-reedit-buffer' must be called instead of
this function."
  ;; If current buffer is specified as buffer, no need to replace.
  (unless (eq (current-buffer) buffer)
    (erase-buffer)
    (insert-buffer buffer))
  (if original
      (save-restriction
        (mail-narrow-to-head)
        (mhc-gnus/decode-buffer)
        (if (mhc-header-valid-p "Content-Type")
            (progn
              (widen)
              (mime-to-mml))
          (rfc2047-decode-region (point-min) (point-max))
          (goto-char (point-max))
          (widen)
          (decode-coding-region (point) (point-max)
                                mhc-default-coding-system))
        (mail-narrow-to-head)
        (mhc-header-delete-header
         (concat "^\\(" (mhc-regexp-opt mhc-draft-unuse-hdr-list) "\\)")
         'regexp))
    (mhc-header-narrowing
      (mhc-header-delete-header
       "^\\(Content-.*\\|Mime-Version\\|User-Agent\\):" 'regexp)))
  (goto-char (point-min))
  (when (re-search-forward "^\r?$" nil t)
    (insert mail-header-separator)))

(defun mhc-gnus-draft-reedit-file (file)
  "Prepare a draft from the FILE.

NOTE: This function designed for original Gnus, not for T-gnus.  When
using T-gnus, `mhc-mime-draft-reedit-file' must be called instead of
this function."
  (erase-buffer)
  (let ((coding-system-for-read 'raw-text-dos)
        (format-alist))
    (insert-file-contents file))
  (mhc-gnus-draft-reedit-buffer (current-buffer) t))

(defun mhc-gnus-draft-translate ()
  "Convert a message in this current buffer to a MIME message.
A input text in this current buffer must be written in MML, a language
for describing MIME parts.

NOTE: This function designed for original Gnus, not for T-gnus.  When
using T-gnus, `mhc-mime-draft-translate' must be called instead of this
function."
  (message-encode-message-body)
  (save-restriction
    (message-narrow-to-headers)
    (message-generate-headers '(Date From Lines))
    (message-remove-header message-ignored-mail-headers t)
    (let ((mail-parse-charset message-default-charset))
      (rfc2047-encode-message-header)))
  (save-excursion
    (goto-char (point-min))
    (when (search-forward (concat "\n" mail-header-separator "\n") nil t)
      (replace-match "\n\n"))))

(defun mhc-gnus-goto-message (&optional view)
  "Go to a view position on summary buffer."
  (when view
    (gnus-summary-next-page)))

;;; MIME APIs (mhc-mime.el):
(defun mhc-gnus-mime-get-raw-buffer ()
  "Get raw buffer of the current message.
Note: This function is used only when using T-gnus."
  (gnus-summary-select-article)
  gnus-original-article-buffer)

(defun mhc-gnus-mime-get-mime-structure ()
  "Get mime message structure of the current message.
Note: This function is used only when using T-gnus."
  (gnus-summary-select-article)
  gnus-current-headers)


;; modify Gnus original functions for cursor control.
(eval-after-load "gnus"
  '(defadvice gnus-summary-position-point
     (around mhc-gnus-summary-position-point activate compile)
     (or mhc-gnus/mhc-is-running ad-do-it)))

(let (current-load-list)
  (defadvice gnus-summary-update-mark
    (around mhc-gnus-summary-update-mark activate compile)
    (or mhc-gnus/mhc-is-running ad-do-it)))

;; modify Gnus original commands for manipulate articles.
(let (current-load-list)
  (defadvice gnus-summary-edit-article
    (around mhc-gnus-draft-edit-message activate compile)
    "If MHC is running, call `mhc-modify'."
    (if mhc-gnus/mhc-is-running
        (mhc-modify)
      ad-do-it)))

(let (current-load-list)
  (defadvice gnus-summary-delete-article
    (around mhc-gnus-summary-delete-article activate compile)
    "If MHC is running, call `mhc-delete'."
    (if mhc-gnus/mhc-is-running
        (mhc-delete)
      ad-do-it)))


(provide 'mhc-gnus)
(put 'mhc-gnus 'summary-filename 'mhc-gnus-summary-filename)
(put 'mhc-gnus 'summary-display-article 'mhc-gnus-summary-display-article)
(put 'mhc-gnus 'generate-summary-buffer 'mhc-gnus-generate-summary-buffer)
(put 'mhc-gnus 'insert-summary-contents 'mhc-gnus-insert-summary-contents)
(put 'mhc-gnus 'summary-mode-setup 'mhc-gnus-summary-mode-setup)
(put 'mhc-gnus 'highlight-message 'mhc-gnus-highlight-message)
(put 'mhc-gnus 'goto-message 'mhc-gnus-goto-message)

(defun mhc-gnus/setup-methods ()
  (if (and (boundp 'gnus-version)
           (stringp (symbol-value 'gnus-version))
           (string-match "SEMI" (symbol-value 'gnus-version)))
      (progn
        (require 'mhc-mime)
        (defalias 'mhc-gnus/encode-string 'eword-encode-string)
        (put 'mhc-gnus 'draft-setup-new 'mhc-mime-draft-setup-new)
        (put 'mhc-gnus 'draft-reedit-buffer 'mhc-mime-draft-reedit-buffer)
        (put 'mhc-gnus 'draft-reedit-file 'mhc-mime-draft-reedit-file)
        (put 'mhc-gnus 'draft-translate 'mhc-mime-draft-translate)
        (put 'mhc-gnus 'get-import-buffer 'mhc-mime-get-import-buffer)
        (put 'mhc-gnus 'decode-header 'mhc-mime-decode-header)
        (put 'mhc-gnus 'eword-decode-string 'mhc-mime-eword-decode-string)
        (put 'mhc-gnus 'mime-get-raw-buffer 'mhc-gnus-mime-get-raw-buffer)
        (put 'mhc-gnus 'mime-get-mime-structure 'mhc-gnus-mime-get-mime-structure))
    (defun mhc-gnus/encode-string (string)
      (let ((rfc2047-encoding-type 'mime))
        (rfc2047-encode-string string)))
    (put 'mhc-gnus 'draft-setup-new 'mhc-gnus-draft-setup-new)
    (put 'mhc-gnus 'draft-reedit-buffer 'mhc-gnus-draft-reedit-buffer)
    (put 'mhc-gnus 'draft-reedit-file 'mhc-gnus-draft-reedit-file)
    (put 'mhc-gnus 'draft-translate 'mhc-gnus-draft-translate)
    (put 'mhc-gnus 'get-import-buffer 'mhc-gnus-get-import-buffer)
    (put 'mhc-gnus 'decode-header 'mhc-gnus-decode-header)
    (put 'mhc-gnus 'eword-decode-string 'mhc-gnus-decode-string)
    (put 'mhc-gnus 'mime-get-raw-buffer nil)
    (put 'mhc-gnus 'mime-get-mime-structure nil)))
(mhc-gnus/setup-methods)

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

;;; mhc-gnus.el ends here.
