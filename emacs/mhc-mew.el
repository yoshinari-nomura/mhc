;;; -*- mode: Emacs-Lisp; coding: utf-8 -*-

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;;           Hideyuki SHIRAI <shirai@quickhack.net>
;; Created: 2000/05/10
;; Revised: $Date$


;;; Commentary:

;; This file is a part of MHC, includes MUA backend methods for Mew.

;;; Code:

(require 'mew)

(defcustom mhc-mew-redisplay-import nil
  "*If non-nil, re-display  with no hooks and no x-face when a message imports"
  :group 'mhc
  :type 'boolean)

;; Internal Variables:
(defvar mhc-mew-new-virtual-type (boundp 'mew-regex-summary2)
  "*Mew virtual format type. Non-nil means Mew 3.0.55 or later.")

(defconst mhc-mew/summary-filename-regex
  (if mhc-mew-new-virtual-type
      ".*[^\006\n]+\006 \\+\\([^ ]*\\) \\([0-9]+\\)$"
    ".*\r *\\+\\([^ \t]+\\)[ \t]+\\([^ \t\n]+\\)"))

;; Mew 6.x does not need invisible property at (beginning-of-line)
(defconst mhc-mew/header-string "")
(defconst mhc-mew/header-string-review "")

(defconst mhc-mew/summary-message-alist
  '((mew-summary-mode . mew-message-mode)
    (mew-virtual-mode . mew-message-mode)))

(defconst mhc-mew/cs-m17n
  (if (>= emacs-major-version 20) 'ctext '*ctext*))

(defconst mhc-mew/lc-ascii
  (if (>= emacs-major-version 20) 'ascii 0))

;; Setup function:

;;;###autoload
(defun mhc-mew-setup ()
  (require 'mhc)
  (setq mhc-mailer-package 'mew)
  (mhc-setup)
  (add-hook 'mew-summary-mode-hook 'mhc-mode)
  (add-hook 'mew-virtual-mode-hook 'mhc-mode)
  (add-hook 'mew-quit-hook 'mhc-exit))

(if (fboundp 'mew-match)
    (defalias 'mhc-mew/match-string 'mew-match)
  (defalias 'mhc-mew/match-string 'match-string))

;; Backend methods:

(defun mhc-mew-summary-filename ()
  (let (folder number)
    (save-excursion
      (beginning-of-line)
      (if (not (looking-at mhc-mew/summary-filename-regex))
          ()
        (setq folder (buffer-substring (match-beginning 1) (match-end 1))
              number (buffer-substring (match-beginning 2) (match-end 2)))
        (mhc-summary-folder-to-path folder number)))))


(defun mhc-mew-summary-display-article ()
  "Display the article on the current."
  (mew-summary-display 'force))


(defun mhc-mew-get-import-buffer (get-original)
  (let (mew-use-highlight-x-face
        mew-opt-highlight-x-face
        mew-message-hook
        mew-summary-display-message-filter-hook)
    (if get-original
        (condition-case nil
            (mew-summary-display-asis t)
          (error (mew-summary-display-asis)))
      (if mhc-mew-redisplay-import
          (cond
           ((fboundp 'mew-summary-analyze-again)
            (mew-summary-analyze-again))
           ((fboundp 'mew-summary-display-command)
            (mew-summary-display-command))
           (t (mew-summary-display))))))
  (save-window-excursion
    (if (eq (cdr (assq major-mode mhc-mew/summary-message-alist))
            (progn (other-window 1) major-mode))
        (current-buffer))))


(defun mhc-mew/date-to-buffer (date)
  (mhc-date-format date "%s/%04d/%02d" mhc-base-folder yy mm))


(defun mhc-mew-generate-summary-buffer (date)
  (switch-to-buffer
   (set-buffer
    (mhc-get-buffer-create (mhc-mew/date-to-buffer date))))
  (setq inhibit-read-only t
        buffer-read-only nil
        selective-display t
        selective-display-ellipses nil
        indent-tabs-mode nil)
  (widen)
  (delete-region (point-min) (point-max)))


(defun mhc-mew/schedule-foldermsg (schedule)
  (let ((path (mhc-record-name (mhc-schedule-record schedule))) fld-msg)
    (if (and
         path
         (string-match
          (concat "^"
                  (regexp-quote (file-name-as-directory mhc-mail-path)))
          path))
        (if (fboundp 'mew-summary-parent-id)
            (progn
              (setq fld-msg (concat "+" (substring path (match-end 0))))
              (setq fld (directory-file-name (file-name-directory fld-msg)))
              (setq msg (file-name-nondirectory fld-msg))
              (with-temp-buffer
                (mew-insert-message fld msg mew-cs-text-for-read mew-header-reasonable-size)
                (setq msgid (or (mew-idstr-get-first-id
                                 (mew-header-get-value "X-SC-Record-Id:"))
                                " "))
                (setq ref (or (mew-idstr-get-first-id
                               (mew-header-get-value mew-message-id:))
                              " ")))
              (concat "\r " (directory-file-name (file-name-directory fld-msg))
                      " " (file-name-nondirectory fld-msg)
                      " " msgid " " ref "  "))
          (setq fld-msg (concat "+" (substring path (match-end 0))))
          (concat "\r "
                  (if mhc-mew-new-virtual-type "<> <> \006 ")
                  (directory-file-name (file-name-directory fld-msg))
                  " "
                  (file-name-nondirectory fld-msg)))
      "")))


(defun mhc-mew-insert-summary-contents (inserter)
  (insert (if mhc-tmp-schedule
              mhc-mew/header-string-review mhc-mew/header-string))
  (funcall inserter)
  (insert (mhc-mew/schedule-foldermsg mhc-tmp-schedule)
          "\n"))


(defun mhc-mew-summary-mode-setup (date)
  (make-local-variable 'mew-use-cursor-mark)
  (make-local-variable 'mew-use-highlight-cursor-line)
  (setq mew-use-cursor-mark nil)
  (setq mew-use-highlight-cursor-line nil)
  (let ((mew-virtual-mode-hook nil))
    (mew-virtual-mode))
  (mew-buffers-setup (buffer-name))
  (and (mew-buffer-message)
       (get-buffer-window (mew-buffer-message))
       (window-live-p (get-buffer-window (mew-buffer-message)))
       (delete-window (get-buffer-window (mew-buffer-message))))
  ;; Mew-1.95b104 or later, disable mark highlight
  (when (boundp 'mew-summary-buffer-raw)
    (setq mew-summary-buffer-raw nil))
  ;; Mew 4.x or later
  (when (fboundp 'mew-summary-set-count-line)
    (mew-summary-set-count-line))
  ;; Mew 4.0.69 or later, fake mew-pickable-p()
  (when (fboundp 'mew-vinfo-set-flds)
    (mew-vinfo-set-flds `(,(mhc-mew/date-to-buffer date)
                          ,(format "%s/intersect" mhc-base-folder))))
  (mew-summary-toggle-disp-msg 'off))


;; This function was orignally written by
;; Shun-ichi Goto <gotoh@taiyo.co.jp> (cf. http://www.imasy.org/~gotoh/)
;; Arranged by Hideyuki SHIRAI <shirai@quickhack.net>.
(defun mhc-mew-decode-header ()
  "mew-message-hook function to decode RAW JIS subject in header"
  (condition-case e
      (if (mew-current-get 'cache)
          (let* ((cache (mew-current-get 'cache))
                 (part (mew-current-get 'part))
                 (syntax (mew-cache-decode-syntax cache))
                 (ent (mew-syntax-get-entry syntax part))
                 (ct (mew-syntax-get-ct ent))
                 (buffer-read-only nil))
            (if (not (equal "Message/Rfc822" (car ct)))
                ()                      ; nothing to do
              ;; do only Message/Rfc822 contents
              (save-excursion
                (save-restriction
                  (widen)
                  (goto-char 1)
                  (if (not (re-search-forward "\r?\n\r?\n" nil t))
                      ()                ; no header
                    (narrow-to-region (point-min) (point))
                    (goto-char (point-min))
                    (if (not (re-search-forward "^X-SC-Subject:" nil t))
                        ()
                      (goto-char (point-min))
                      ;; decode raw JIS string
                      (while (< (point) (point-max))
                        (if (looking-at "[^:]+:? *")
                            (goto-char (match-end 0)))
                        (if (and (not (looking-at "[\t\x20-\x7e]+$"))
                                 (equal (mew-find-cs-region
                                         (point)
                                         (save-excursion (end-of-line)
                                                         (point)))
                                        (list mhc-mew/lc-ascii)))
                            ;; decode!
                            (mew-cs-decode-region (point)
                                                  (save-excursion
                                                    (end-of-line)
                                                    (point))
                                                  mhc-mew/cs-m17n))
                        (beginning-of-line)
                        (forward-line 1))
                      ;; re-highlight
                      (mew-highlight-header)
                      (save-excursion
                        (mew-highlight-x-face (point-min) (point-max))))))))))
    (error
     (ding t)
     (message "mhc-message-decode-header: %s" (or (cdr e) "some error!")))))


(defun mhc-mew-draft-setup-new ()
  (make-local-variable 'mail-header-separator)
  (setq mail-header-separator mew-header-separator)
  (goto-char (point-min))
  (mew-header-set (concat mew-header-separator "\n")))


(defun mhc-mew-draft-reedit-buffer (buffer original)
  ;; If current buffer is specified as buffer, no need to replace.
  (unless (eq (current-buffer) buffer)
    (erase-buffer)
    (insert-buffer buffer))
  (make-local-variable 'mail-header-separator)
  (setq mail-header-separator mew-header-separator)
  (mhc-header-narrowing
    (mhc-header-delete-header "x-mew"))
  (goto-char (point-min))
  (re-search-forward "^$" nil 'limit)
  (or (= (current-column) 0) (insert "\n"))
  (mew-header-set mew-header-separator)
  (goto-char (point-min)))


(defun mhc-mew-draft-reedit-file (file)
  (erase-buffer)
  (insert-file-contents file)
  (make-local-variable 'mail-header-separator)
  (setq mail-header-separator mew-header-separator)
  (goto-char (point-min))
  (and (re-search-forward "^$" nil t)
       (save-excursion
         (save-restriction
           (narrow-to-region (point-min) (point))
           (goto-char (point-min))
           (let ((mew-field-spec nil)
                 (mew-decode-broken nil))
             (mew-decode-rfc822-header t)))))
  (mhc-header-narrowing
    (mhc-header-delete-header "x-mew"))
  (goto-char (point-min))
  (re-search-forward "^$" nil 'limit)
  (or (= (current-column) 0) (insert "\n"))
  (mew-header-set mew-header-separator)
  (goto-char (point-min)))


(defun mhc-mew-highlight-message (for-draft)
  (when (mew-header-end)
    (mew-highlight-header))
  ;; Mew-1.95b104 or later, not have functions.
  (when (and (fboundp 'mew-highlight-url)
             (fboundp 'mew-highlight-body))
    (mew-highlight-url)
    (mew-highlight-body)))


(defun mhc-mew-draft-translate ()
  (let ((bufstr (buffer-substring (point-min) (point-max)))
        (case-fold-search t)
        ct cte boundary beg end)
    (condition-case nil
        (progn
          (mhc-header-narrowing
            ;; Mew can't encode Mime-Version ?
            (setq ct (mhc-header-get-value "content-type"))
            (setq cte (mhc-header-get-value "content-transfer-encoding"))
            (mhc-header-delete-header "mime-version")
            (mhc-header-delete-header "content-type")
            (mhc-header-delete-header "content-transfer-encoding"))
          (when (and ct (string-match "^multipart/" ct)
                     (or (string-match "boundary=\"\\([^\"]+\\)\"" ct)
                         (string-match "boundary=\\(.+\\)" ct)))
            (setq boundary (regexp-quote (mhc-mew/match-string 1 ct)))
            (let ((case-fold-search nil))
              (goto-char (point-min))
              (unless (and boundary
                           (re-search-forward (concat "^--" boundary "$") nil t)
                           (re-search-forward (concat "^--" boundary "--$") nil t))
                ;; looks like Broken multi-part message.
                (setq boundary nil))))
          (if (null (mew-header-end))
              (mhc-header-narrowing
                (mew-header-encode-region (point-min) (point-max)))
            (mew-header-encode-region (point-min) (mew-header-end))
            (mew-header-clear)
            (insert "\n"))
          (if (null boundary)
              ;; text/plain
              (progn
                (mhc-header-narrowing
                  (mhc-header-put-value "Mime-Version" "1.0"))
                (mhc-mew/make-message))
            ;; Multipart
            (mhc-header-narrowing
              (mhc-header-put-value "Mime-Version" "1.0")
              (mhc-header-put-value "Content-Type" (or ct mew-ct-txt))
              (mhc-header-put-value "Content-Transfer-Encoding" (or cte mew-7bit)))
            (when (and (re-search-forward (concat "^--" boundary "$") nil t)
                       (forward-line)
                       (setq beg (point))
                       (re-search-forward (concat "\n--" boundary "\\(--\\)?$") nil t)
                       (setq end (match-beginning 0)))
              ;; first sub-part
              (goto-char beg)
              (when (or (looking-at "^content-type: +text/plain")
                        (looking-at "^$"))
                (save-excursion
                  (save-restriction
                    (narrow-to-region beg end)
                    (mhc-mew/make-message)))))))
      (error
       (let ((buffer-read-only nil)
             (inhibit-read-only t))
         (delete-region (point-min) (point-max))
         (insert bufstr)
         (goto-char (point-min))
         (ding t)
         (error "%s"
                (or (and (fboundp 'mew-tinfo-get-encode-err)
                         (mew-tinfo-get-encode-err))
                    "Draft buffer has some illegal headers. Please fix it.")))))))

(defun mhc-mew/make-message ()
  (mew-charset-sanity-check (point-min) (point-max))
  (goto-char (point-min))
  (re-search-forward "^$" nil t)
  (forward-line)
  (let* ((charset (or (and mhc-default-coding-system (mew-cs-to-charset mhc-default-coding-system))
                      (mew-charset-guess-region (point) (point-max))
                      mew-us-ascii))
         (cte (or (mew-charset-to-cte charset) mew-b64))
         (switch mew-prog-mime-encode-text-switch)
         beg opt file)
    (mhc-header-narrowing
      (mhc-header-put-value "Content-Type"
                            (concat "Text/Plain; charset=" charset))
      (mhc-header-put-value "Content-Transfer-Encoding" cte))
    (if (mew-case-equal cte mew-7bit)
        ()
      (goto-char (point-min))
      (re-search-forward "^$" nil t)
      (forward-line)
      (setq beg (point))
      (mew-cs-encode-region beg (point-max) (mew-charset-to-cs charset))
      (cond
       ((mew-case-equal cte mew-8bit)
        ())
       ((and (mew-case-equal cte mew-b64) (fboundp 'base64-encode-region))
        (goto-char beg)
        (while (search-forward "\n" nil t) (replace-match "\r\n"))
        (base64-encode-region beg (point-max))
        (goto-char (point-max))
        (insert "\n"))
       ((mew-which-exec mew-prog-mime-encode)
        (setq opt (mew-prog-mime-encode-get-opt cte switch))
        (if (null opt)
            (mew-encode-error (concat "Unknown CTE: " cte))
          (setq file (mew-make-temp-name))
          (mew-frwlet
              mew-cs-dummy mew-cs-text-for-write
            ;; NEVER use call-process-region for privacy reasons
            (write-region beg (point-max) file nil 'no-msg))
          (delete-region beg (point-max)))
        (mew-piolet
            mew-cs-text-for-read mew-cs-dummy
          (apply (function call-process) mew-prog-mime-encode
                 file t nil opt))
        (if (file-exists-p file)
            (delete-file file)))
       (t (mew-encode-error (concat mew-prog-mime-encode " doesn't exist")))))))


(defconst mhc-mew-header-decode-regex "\\(=\\?[^? \t]+\\?.\\?[^? \t]+\\?=\\)")

(defun mhc-mew-eword-decode-string (string)
  (let ((ret "") tmpstr)
    (while (string-match "\n" string)
      (setq string (replace-match "" nil nil string)))
    (while (string-match (concat mhc-mew-header-decode-regex
                                 "\\([ \t]+\\)"
                                 mhc-mew-header-decode-regex)
                                 string)
      (setq string (replace-match "\\1\\3" nil nil string)))
    (while (string-match "[ \t\][ \t\]+" string)
      (setq string (replace-match " " nil nil string)))
    (while (not (string= string ""))
      (if (not (string-match mew-header-decode-regex string))
          (setq ret (concat ret string) string "")
        (setq tmpstr (substring string (match-end 0)))
        (setq ret (concat ret
                          (substring string 0 (match-beginning 0))
                          (mew-header-decode (mhc-mew/match-string 1 string)
                                             (mhc-mew/match-string 2 string)
                                             (mhc-mew/match-string 3 string))))
        (setq string tmpstr)))
    ret))


(defun mhc-mew-decode-rfc822-header ()
  (mew-decode-rfc822-header)
  (mew-header-goto-end)
  (mew-header-arrange (point-min) (point)))

(defun mhc-mew-goto-message (&optional view)
  "Go to a view position on summary buffer."
  (when (fboundp 'mew-summary-goto-message)
    (mew-summary-goto-message))
  (when view
    (condition-case nil
        (mew-summary-display 'force)
      (error
       (mew-summary-display)))))

(provide 'mhc-mew)
(put 'mhc-mew 'summary-filename 'mhc-mew-summary-filename)
(put 'mhc-mew 'summary-display-article 'mhc-mew-summary-display-article)
(put 'mhc-mew 'get-import-buffer 'mhc-mew-get-import-buffer)
(put 'mhc-mew 'highlight-message 'mhc-mew-highlight-message)
(put 'mhc-mew 'draft-mode 'mhc-mew-draft-mode)
(put 'mhc-mew 'generate-summary-buffer 'mhc-mew-generate-summary-buffer)
(put 'mhc-mew 'insert-summary-contents 'mhc-mew-insert-summary-contents)
(put 'mhc-mew 'summary-search-date 'mhc-mew-summary-search-date)
(put 'mhc-mew 'summary-mode-setup 'mhc-mew-summary-mode-setup)
(put 'mhc-mew 'draft-setup-new 'mhc-mew-draft-setup-new)
(put 'mhc-mew 'draft-reedit-buffer 'mhc-mew-draft-reedit-buffer)
(put 'mhc-mew 'draft-reedit-file 'mhc-mew-draft-reedit-file)
(put 'mhc-mew 'draft-translate 'mhc-mew-draft-translate)
(put 'mhc-mew 'eword-decode-string 'mhc-mew-eword-decode-string)
(put 'mhc-mew 'decode-header 'mhc-mew-decode-rfc822-header)
(put 'mhc-mew 'goto-message 'mhc-mew-goto-message)

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

;;; mhc-mew.el ends here.
