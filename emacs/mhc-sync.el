;;; -*- emacs-lisp -*-
;; mhc-sync.el -- mhc-sync (ruby script) interface
;;
;; Author:  Hideyuki SHIRAI <shirai@quickhack.net>
;;
;; Created: 2000/06/12
;; Revised: $Date: 2002/11/11 05:27:15 $

;;; Commentary:

;; This file is a part of MHC, includes backend functions to
;; manipulate schedule files.


;;; Customize Variables:
(defcustom mhc-sync-id nil
  "*Identical id of mhc-sync (-x option)."
  :group 'mhc
  :type 'string)

(defcustom mhc-sync-remote nil
  "*Remote server repository of mhc-sync ([user@]remote.host[:dir])."
  :group 'mhc
  :type 'string)
  
(defcustom mhc-sync-localdir nil
  "*Local repository directory of mhc-sync (-r option)."
  :group 'mhc
  :type 'string)

(defcustom mhc-sync-coding-system
  (if (>= emacs-major-version 20) 'undecided '*autoconv*)
  "*Default coding system for process of mhc-sync."
  :group 'mhc
  :type 'symbol)


;;; Interanal variabiles:
(defconst mhc-sync/passwd-regexp "password:\\|passphrase:\\|Enter passphrase")

(defvar mhc-sync/process nil)

(defvar mhc-sync/req-passwd nil)


;;; Code:
(defun mhc-sync/backup-and-remove (file &optional offline)
  "Backend function to remove FILE."
  (let ((file (expand-file-name file))
	(new-path (expand-file-name
		   "trash"
		   (mhc-summary-folder-to-path mhc-base-folder))))
    (or (file-directory-p new-path)
	(make-directory new-path))
    (rename-file file (mhc-misc-get-new-path new-path))))

(defun mhc-sync/start-process (&optional full)
  (cond
   ((not (and (stringp mhc-sync-remote) (stringp mhc-sync-id)))
    (message "No remote server specified.")
    nil)
   ((processp mhc-sync/process)
    (message "another mhc-sync running.")
    nil)
   (t
    (let ((buf (mhc-get-buffer-create " *mhc-sync*"))
	  (ldir (expand-file-name (or mhc-sync-localdir "~/Mail/schedule"))))
      (mhc-window-push)
      (pop-to-buffer buf)
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq buffer-read-only t)
      (message "mhc-sync...")
      (setq mhc-sync/req-passwd t)
      (setq mhc-sync/process
	    (apply (function start-process)
		   "mhc-sync" buf "mhc-sync"
		   (list "-x" mhc-sync-id "-r" ldir mhc-sync-remote)))
      (set-process-coding-system mhc-sync/process mhc-sync-coding-system)
      (set-process-filter mhc-sync/process 'mhc-sync/filter)
      (set-process-sentinel mhc-sync/process 'mhc-sync/sentinel)
      (if (featurep 'xemacs)
	  (while mhc-sync/process
	    (accept-process-output))
	(while mhc-sync/process
	  (sit-for 0.1)
	  (discard-input)))
      (sit-for 1)
      (mhc-window-pop)
      (or (and (mhc-summary-buffer-p)
	       (mhc-rescan-month mhc-default-hide-private-schedules))
	  (and (mhc-calendar-p) (mhc-calendar-rescan)))
      t))))

(defun mhc-sync/filter (process string)
  (if (bufferp (process-buffer process))
      (let ((obuf (buffer-name)))
	(unwind-protect
	    (progn
	      (set-buffer (process-buffer process))
	      (let ((buffer-read-only nil)
		    passwd)
		(goto-char (point-max))
		(insert string)
		(cond
		 ((and mhc-sync/req-passwd
		       (string-match mhc-sync/passwd-regexp string))
		  (setq passwd (mhc-misc-read-passwd string))
		  (process-send-string process (concat passwd "\n")))
		 ((string-match "---------------------" string)
		  (setq mhc-sync/req-passwd nil)))))
	  (if (get-buffer obuf)
	      (set-buffer obuf))))))

(defun mhc-sync/sentinel (process event)
  (when (bufferp (process-buffer process))
    (pop-to-buffer (process-buffer process))
    (let ((buffer-read-only nil))
      (goto-char (point-max))
      (insert "<<<transfer finish>>>")))
  (setq mhc-sync/process nil))


(provide 'mhc-sync)
(put 'mhc-sync 'remove 'mhc-sync/backup-and-remove)
(put 'mhc-sync 'sync   'mhc-sync/start-process)

;;; Copyright Notice:

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

;; mhc-sync.el ends here
