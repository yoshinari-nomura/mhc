;;; -*- mode: Emacs-Lisp; coding: utf-8 -*-

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;; Created: 2000/04/30
;; Revised: $Date$


;;; Commentary:

;; This file is a part of MHC, and includes defintions of global
;; confiration variables.


;;; Code:
(require 'mhc-compat)


;;; Constants:
(defconst mhc-version "mhc 0.25 + snap (CHECKOUT-FROM-CVS)")


;;; Configration Variables:
(defgroup mhc nil
  "Various sorts of MH Calender."
  :group 'mail)

(defcustom mhc-mailer-package 'mew
  "*Variable to set your favorite mailer."
  :group 'mhc
  :type '(radio (const :tag "Mew" mew)
                (const :tag "Wanderlust" wl)
                (const :tag "Gnus" gnus)))

(defcustom mhc-base-folder "+schedule"
  "*Base foler of MHC"
  :group 'mhc
  :type 'string)

(defcustom mhc-mail-path
  (expand-file-name
   (if (and (boundp 'mew-mail-path) mew-mail-path) mew-mail-path "~/Mail"))
  "*Base directory your mailer recognized as `+'"
  :group 'mhc
  :type 'directory)

(defcustom mhc-schedule-file (expand-file-name "~/.schedule")
  "*MHC DB file which contains holiday and anniversary settings."
  :group 'mhc
  :type 'file)

(defcustom mhc-start-day-of-week 0
  "*Day of the week as the start of the week."
  :group 'mhc
  :type '(choice (const :tag "Sunday" 0)
                 (const :tag "Monday" 1)
                 (const :tag "Tuesday" 2)
                 (const :tag "Wednesday" 3)
                 (const :tag "Thursday" 4)
                 (const :tag "Friday" 5)
                 (const :tag "Saturday" 6)))

(defcustom mhc-insert-calendar t
  "*If non nil value, display vertical calender."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-vertical-calendar-length 3
  "*Length of vertical calendar in summary buffer."
  :group 'mhc
  :type '(radio (integer :tag "Show length (current month is center)" 3)
                (cons (integer :tag "             Show length" 3)
                      (integer :tag "Length of before current" 1))))

(defcustom mhc-insert-todo-list t
  "*If non nil value, display TODO list."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-insert-memo-list t
  "*If non nil value, display MEMO list."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-default-coding-system
  (if (>= emacs-major-version 20) 'utf-8-unix '*iso-2022-ss2-7*)
  "*Default coding system for MHC schedule files."
  :group 'mhc
  :type 'symbol)

(defcustom mhc-default-hide-private-schedules nil
  "*If non-nil value, hide private schedules."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-category-as-private '("private")
  "*String list of private categories."
  :group 'mhc
  :type '(repeat (string :tag "Category")))

(defcustom mhc-default-network-status t
  "*Flag of the default network status."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-show-network-status t
  "*Flag to show the network status."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-use-cache t
  "*Flag to decide whether to use cache or not."
  :group 'mhc
  :type '(radio (const :tag "Use" t)
                (const :tag "Lazy check" 0)
                (const :tag "No use" nil)))

(defcustom mhc-use-wide-scope nil
  "*Wide scope method in summary mode."
  :group 'mhc
  :type '(radio (const :tag "No use" nil)
                (const :tag "Complete week scope" week)
                (const :tag "Wide week scope" wide)
                (integer :tag "Scope wide size (>=0)" 3)))

(defcustom mhc-default-alarm "5 minute"
  "*Default alarm string in making draft."
  :group 'mhc
  :type 'string)

(defcustom mhc-ask-alarm nil
  "*If non-nil value, ask the alarm string in making draft."
  :group 'mhc
  :type 'boolean)

(provide 'mhc-vars)

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

;;; mhc-vars.el ends here
