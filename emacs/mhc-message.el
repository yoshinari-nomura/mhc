;;; mhc-message.el --- Message major mode in MHC.

;; Copyright (C) 2014 MHC development team.

;; Author: Yoshinari Nomura <nom@quickhack.net>
;; Keywords: calendar

;; This file is NOT part of GNU Emacs.

;;; License:

;; You can redistribute it and/or modify it under the terms of
;; The BSD 3-Clause License.  You can check the details from:
;;   https://github.com/yoshinari-nomura/mhc/blob/master/COPYRIGHT

;;; Commentary:

;; This file is a part of MHC.  mhc-message major mode in MHC.

;;; Code:

(defcustom mhc-message-mode-hook nil
  "*Hook run in mhc message mode buffers."
  :group 'mhc
  :type 'hook)

(defvar mhc-message-mode-map nil)
(setq mhc-message-mode-map (make-sparse-keymap))
(define-key mhc-message-mode-map " " 'mhc-message-scroll-page-forward)

(defvar mhc-message-end-of-messge-marker "[End of message]")

(define-derived-mode mhc-message-mode
  text-mode
  "MHC-Msg"
  "Major mode for viewing schdule files of MHC."
  (save-excursion
    (mhc-decode-header)
    (goto-char (point-max))
    (unless (re-search-backward
             (regexp-quote mhc-message-end-of-messge-marker)
             (- (point) (length mhc-message-end-of-messge-marker))
             t)
      (unless (bolp) (insert "\n"))
      (insert mhc-message-end-of-messge-marker))
    (mhc-highlight-message))
  ;; (setq mhc-message-mode-called-count (1+ mhc-message-mode-called-count))
  ;; (message "mhc-message-mode-called-count: %d" mhc-message-mode-called-count)
  ;; )
  ;; (set (make-local-variable 'scroll-error-top-bottom) t)
  ;; (run-hooks 'mhc-message-mode-hook)
  )

;; user interface

(defun mhc-message-scroll-page-forward (&optional lines)
  "Scroll text of selected MHC message window upward LINES.
If LINES is omitted or nil, scroll upward by a near full screen."
  (interactive)
  (unless (ignore-errors (scroll-up lines) t)
    (message "End of buffer")))

(defun mhc-message-scroll-page-backward (&optional lines)
  "Scroll text of selected MHC message window down LINES.
If LINES is omitted or nil, scroll down by a near full screen."
  (interactive)
  (unless (ignore-errors (scroll-down lines) t)
    (message "Beginning of buffer")))

;; file signature

(defvar mhc-message-current-signature nil)
(make-variable-buffer-local 'mhc-message-cache-signature)

(defun mhc-message-file-signature (file-name)
  (let ((file-path (and (stringp file-name)
                        (expand-file-name file-name))))
    (and file-path
         (file-exists-p file-path)
         (cons file-path
               (nth 5 (file-attributes file-path))))))

(defun mhc-message-update-signature (file-name)
  (let ((file-signature (mhc-message-file-signature file-name)))
    (if (equal mhc-message-current-signature file-signature)
        nil ;; not updated
      (setq mhc-message-current-signature file-signature)
      t ;; updated
      )))

;; message setup and update

(defun mhc-message-create (buffer-or-name &optional file-name)
  "Create the mhc-message-mode buffer specified by BUFFER-OR-NAME.
This is similar to `get-buffer-create'.
If FILE-NAME is non-nil, the buffer is filled with the content of FILE-NAME."
  (let ((buf (get-buffer-create buffer-or-name)))
    (with-current-buffer buf
      (mhc-message-clear)
      (if file-name (mhc-message-update file-name))
      (mhc-message-mode)
      (set-buffer-modified-p nil))
    buf))

(defun mhc-message-update (file-name &optional buffer-or-name)
  "Replace buffer content by FILE-NAME in BUFFER-OR-NAME."
  (let ((buf (or buffer-or-name (current-buffer))))
    (with-current-buffer buf
      (when (mhc-message-update-signature file-name)
        (mhc-message-clear)
        (mhc-insert-file-contents-as-coding-system
         mhc-default-coding-system file-name)))
    buf))

(defun mhc-message-clear (&optional buffer-or-name)
  "Clear current buffer content.
If BUFFER-OR-NAME is specified, clear the content of BUFFER-OR-NAME.
Returns the designated buffer."
  (let ((buf (or buffer-or-name (current-buffer))))
    (with-current-buffer buf
      (setq buffer-read-only nil
            inhibit-read-only t)
      (set-visited-file-name nil)
      (goto-char (point-min))
      (erase-buffer))
    buf))

(provide 'mhc-message)

;;; mhc-message.el ends here
