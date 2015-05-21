;;; mhc-mua.el --- MUA backend for MHC

;; Author:  Yoshinari Nomura <nom@quickhack.net>
;; Created: 2014-09-20

;;; Commentary:

;; This file is a part of MHC, backend methods for Generic MUA.

;;; Code:

(defconst mhc-mua/summary-filename-regex
  ".*\r *\\+\\([^ \t]+\\)[ \t]+\\([^ \t\n]+\\)")

;; Setup function:

(eval-and-compile
  (autoload 'rfc2047-decode-region "rfc2047")
  (autoload 'rfc2047-decode-string "rfc2047")
  ;; (autoload 'eword-encode-string "eword-encode")
  ;; (autoload 'rfc2047-encode-message-header "rfc2047")
  ;; (autoload 'rfc2047-encode-string "rfc2047")
  )

;;;###autoload
(defun mhc-mua-setup ()
  (require 'mhc)
  (setq mhc-mailer-package 'mua)
  (mhc-setup)
  (add-hook 'mhc-summary-mode-hook 'mhc-mode))

;;; Backend methods:

;;; for mhc-summary

(defun mhc-mua-summary-filename ()
  (let (folder number)
    (save-excursion
      (beginning-of-line)
      (if (not (looking-at mhc-mua/summary-filename-regex))
          ()
        (buffer-substring (match-beginning 2) (match-end 2))))))

(defun mhc-mua-summary-display-article ()
  "Display the current article pointed in summary."
  (let ((file (mhc-mua-summary-filename)))
    (if (not (and (stringp file) (file-exists-p file)))
        (message "File does not exist.")
      (mhc-window-push)
      ;; (view-file-other-window file)
      (pop-to-buffer (get-buffer-create "*MHC message*"))
      ;; eword decode
      (let ((buffer-read-only nil))
        (goto-char (point-min))
        (erase-buffer)
        (insert-file-contents file)
        (mhc-header-narrowing
          (mhc-header-delete-header
           "^\\(Content-.*\\|Mime-Version\\|User-Agent\\):" 'regexp))
        (mhc-header-delete-empty-header
         "^X-SC-.*:" 'regexp)
        (mhc-message-mode)
        (mhc-message-set-file-name file))
      ;; (setq view-exit-action 'mhc-calendar-view-exit-action)
      (set-visited-file-name nil)
      ;; (rename-buffer (file-name-nondirectory file) 'unique)
      ;; (run-hooks 'mhc-calendar-view-file-hook)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      )))

(defun mhc-mua-get-import-buffer (&optional get-raw)
  "Return a buffer visiting import article.
If GET-RAW is non-nil, return a cons of buffer: car keeps a raw
message and cdr keeps a visible message."
  (let ((buffer
         (or
          (save-window-excursion
            (let ((mode (progn (other-window 1) major-mode)))
              (if (or
                   (eq mode 'mew-message-mode)
                   (eq mode 'mhc-message-mode))
                  (current-buffer))))
          (current-buffer))))
    ;; XXX get-raw is gone soon
    (if get-raw
        (cons buffer buffer)
      buffer)))

(defun mhc-mua-generate-summary-buffer (name-or-date)
  "Generate a summary buffer for DATE-OR-DATE, and change current buffer to it."
  (switch-to-buffer
   (set-buffer
    (mhc-get-buffer-create
     (if (stringp name-or-date)
         name-or-date
       (mhc-date-format name-or-date "%04d-%02d" yy mm)))))
  (setq inhibit-read-only t
        buffer-read-only nil
        selective-display t
        selective-display-ellipses nil
        indent-tabs-mode nil)
  (widen)
  (delete-region (point-min) (point-max)))

(defun mhc-mua/schedule-foldermsg (schedule)
  (concat "\r +MHC " (mhc-record-name (mhc-schedule-record schedule))))

(defun mhc-mua-insert-summary-contents (inserter)
  (funcall inserter)
  (insert (mhc-mua/schedule-foldermsg mhc-tmp-schedule) "\n"))

(defun mhc-mua-summary-mode-setup (date)
  (mhc-summary-mode))


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

(defalias 'mhc-mua-eword-decode-string 'rfc2047-decode-string)

(defun mhc-mua-decode-header ()
  (save-restriction
    (mhc-header-narrowing
      (rfc2047-decode-region (point-min) (point-max)))))

;;; for mhc-draft

;;; for mhc-calendar

(defun mhc-mua-goto-message (&optional view)
  "Go to a view position on summary buffer."
  (when view
    (mhc-summary-display)))

(provide 'mhc-mua)
(put 'mhc-mua 'summary-filename        'mhc-mua-summary-filename)
(put 'mhc-mua 'summary-display-article 'mhc-mua-summary-display-article)
(put 'mhc-mua 'get-import-buffer       'mhc-mua-get-import-buffer)
(put 'mhc-mua 'highlight-message       'mhc-mua-highlight-message)
(put 'mhc-mua 'draft-mode              'mhc-mua-draft-mode)
(put 'mhc-mua 'generate-summary-buffer 'mhc-mua-generate-summary-buffer)
(put 'mhc-mua 'insert-summary-contents 'mhc-mua-insert-summary-contents)
(put 'mhc-mua 'summary-search-date     'mhc-mua-summary-search-date)
(put 'mhc-mua 'summary-mode-setup      'mhc-mua-summary-mode-setup)
(put 'mhc-mua 'draft-setup-new         'mhc-mua-draft-setup-new)
(put 'mhc-mua 'eword-decode-string     'mhc-mua-eword-decode-string)
(put 'mhc-mua 'decode-header           'mhc-mua-decode-header)
(put 'mhc-mua 'goto-message            'mhc-mua-goto-message)

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
