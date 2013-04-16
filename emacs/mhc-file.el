;;; -*- mode: Emacs-Lisp; coding: utf-8 -*-

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Created: 2000/05/01
;; Revised: $Date$


;;; Comments:

;; This file is a part of MHC, and includes functions to manipulate
;; files of schedules.


;;; About Backend:

;; このライブラリは、実際にファイルを操作するバックエンドを呼び出すこ
;; とによって動作する。バックエンドは、以下のようなメソッドを提供する
;; ことが期待されている。
;;
;;     (mhc-foo/init)
;;          ネットワークの状態に依存しない初期化処理を行う関数
;;
;;     (mhc-foo/exit)
;;          ネットワークの状態に依存しない終了処理を行う関数
;;
;;     (mhc-foo/open &optional OFFLINE)
;;          ネットワークの状態に依存する初期化処理を行う関数
;;
;;     (mhc-foo/close &optional OFFLINE)
;;          ネットワークの状態に依存する終了処理を行う関数
;;
;;     (mhc-foo/sync)
;;          スケジュールファイルの同期を取る関数
;;
;;     (mhc-foo/add FILENAME &optional OFFLINE)
;;          ファイルを追加を通知する関数
;;          (ファイルの実体は追加された後に呼び出される)
;;
;;     (mhc-foo/modify FILENAME &optional  OFFLINE)
;;          ファイルの変更を通知する関数
;;          (ファイルの実体が変更された後に呼び出される)
;;
;;     (mhc-foo/remove FILENAME &optional OFFLINE)
;;          ファイルを削除する関数
;;          (ファイルの実体は *削除されずに* 呼び出される)
;;
;; これらのメソッドを適切に定義し、更に以下のような宣言を付け加える。
;;
;;     (provide 'mhc-foo)
;;     (put 'mhc-foo 'init   'mhc-foo/init)
;;     (put 'mhc-foo 'exit   'mhc-foo/exit)
;;     (put 'mhc-foo 'open   'mhc-foo/open)
;;     (put 'mhc-foo 'close  'mhc-foo/close)
;;     (put 'mhc-foo 'sync   'mhc-foo/sync)
;;     (put 'mhc-foo 'add    'mhc-foo/add)
;;     (put 'mhc-foo 'modify 'mhc-foo/modify)
;;     (put 'mhc-foo 'remove 'mhc-foo/remove)
;;
;; メソッドの関数名は任意に選ぶことができる。
;;
;; また、メソッドの定義は省略することができる。省略されたメソッドは、
;; 関数 mhc-file/true によって置換され、その処理は常に成功したものと見
;; なされる。


;;; Definition
(require 'mhc-compat)
(require 'mhc-vars)


;;; Global Variables
(defcustom mhc-file-method 'mhc-sync
  "*Variable to specify the method to control schdule files."
  :group 'mhc
  :type '(radio (const :tag "Backup and remove" mhc-sync)
                (const :tag "CVS" mhc-cvs)
                (symbol :tag "Other")))

(defcustom mhc-file-sync-enable-offline nil
  "*If non-nil, enable mhc-file-sync when status is offline."
  :group 'mhc
  :type '(radio (const :tag "Disable when offline" nil)
                (const :tag "Enable when offline" t)))

;;; Internal Variables
(defvar mhc-file/offline (not mhc-default-network-status)
  "Keep current line status.")


;;; Codes
(defun mhc-file/true (&rest arguments)
  "Dummy function for undefind backend functions."
  t)

(defconst mhc-file/backend-method-list
  '(init exit open close sync add modify remove))

;; To suprress byte compile warnings.
(eval-when-compile
  (mapcar (lambda (s)
            (let ((f (intern (concat "mhc-file/" (symbol-name s)))))
              (or (fboundp f) (fset f 'mhc-file/true))))
          mhc-file/backend-method-list))

(defun mhc-file-setup (&optional method)
  "Initialize backend to manipulate files."
  (require (or method mhc-file-method))
  (mapcar (lambda (s)
            (fset (intern (concat "mhc-file/" (symbol-name s)))
                  (or (get mhc-file-method s) 'mhc-file/true)))
          mhc-file/backend-method-list)
  (and (mhc-file/init)
       (mhc-file/open mhc-file/offline)))

(defun mhc-file-exit ()
  "Exit backend to manipulate files."
  (and (mhc-file/close mhc-file/offline)
       (mhc-file/exit)))

(defmacro mhc-file-add (file)
  `(mhc-file/add ,file mhc-file/offline))
(defmacro mhc-file-modify (file)
  `(mhc-file/modify ,file mhc-file/offline))
(defmacro mhc-file-remove (file)
  `(mhc-file/remove ,file mhc-file/offline))

(defcustom mhc-file-line-status-strings
  '(" mhc[offline]" . " mhc[ONLINE]")
  "Strings to describe MHC network status."
  :group 'mhc
  :type '(choice
          (const :tag "Long format" (" mhc[offline]" . " mhc[ONLINE]"))
          (const :tag "Short format" (" Mhc" . " MHC"))
          (cons :tag "User definition"
                (string :tag "String for offline")
                (string :tag "String for online")))
  :set (lambda (symbol value)
         (set-default symbol value)
         (if (assq 'mhc-mode minor-mode-alist)
             (setcdr (assq 'mhc-mode minor-mode-alist) (list (mhc-file-line-status))))
         (force-mode-line-update)))

(defun mhc-file-line-status ()
  "Return status string for mode line."
  (if mhc-show-network-status
      (if mhc-file/offline
          (car mhc-file-line-status-strings)
        (cdr mhc-file-line-status-strings))))

(defun mhc-file-toggle-offline (&optional full set-to no-sync)
  "*Toggle line status of file manipulation backend."
  (interactive (list current-prefix-arg (not mhc-file/offline)))
  (let ((previous mhc-file/offline))
    (setq mhc-file/offline set-to)
    (if (assq 'mhc-mode minor-mode-alist)
        (setcdr (assq 'mhc-mode minor-mode-alist)
                (list (mhc-file-line-status))))
    (if mhc-file/offline
        (message "mhc-file is offline.")
      (if (and (not no-sync)
               previous
               (y-or-n-p "Sync schedule files right now ? "))
          (mhc-file-sync full))
      (message "mhc-file is online."))))


(defun mhc-file-sync (&optional full)
  "*Sync schedule files."
  (interactive "P")
  (if (and mhc-file/offline (not mhc-file-sync-enable-offline))
      (message "\"M-x mhc-file-toggle-offline\" first.")
    (message "mhc file sync...")
    (when (mhc-file/sync full)
      (message "mhc file sync...done"))))


;; almost same as (make-directory dirname t)
(defun mhc-file-make-directory (dirname)
  (if (file-directory-p dirname)
      t
    (if (mhc-file-make-directory
         (directory-file-name (file-name-directory (directory-file-name dirname))))
        (progn
          (make-directory (directory-file-name dirname))
          (mhc-file-add (file-name-as-directory dirname))
          t))))



(provide 'mhc-file)

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

;;; mhc-file.el ends here.
