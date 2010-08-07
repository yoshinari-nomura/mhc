;;; mhc-bm.el -- Bitmap stuff for MHC.

;; Author:  Yuuichi Teranishi <teranisi@gohome.org>
;;
;; Created: 2000/05/27
;; Revised: $Date: 2002/11/11 05:27:14 $

(require 'bitmap)
(require 'mhc-face)

(defcustom mhc-bm-icon-alist
  '(("Conflict"    . ("Conflict.xbm" "Yellow"))
    ("Private"     . ("Private.xbm" "HotPink"))
    ("Holiday"     . ("Holiday.xbm" "OrangeRed" "White"))
    ("Todo"         .("CheckBox.xbm" "Red"))
    ("Done"        . ("CheckedBox.xbm" "Red"))
    ("Link"       .  ("Link.xbm" "Gray")))
  "*Alist to define icons.
Each element should have the form
 (NAME . (ICON-FILE FG BG))
It defines icon named NAME with FG and BG color created from ICON-FILE.
FG and BG can be omitted (default color is used).
Example:
  '((\"Holiday\"     . (\"Holiday.xbm\" \"OrangeRed\" \"White\"))
    (\"Work\"        . (\"Business.xbm\" \"Tan\"))
    (\"Private\"     . (\"Private.xbm\" \"HotPink\"))
    (\"Anniversary\" . (\"Anniversary.xbm\" \"SkyBlue\"))
    (\"Birthday\"    . (\"Birthday.xbm\"))
    (\"Other\"       . (\"Other.xbm\" \"Red\"))
    (\"Todo\"         .(\"CheckBox.xbm\" \"Red\"))
    (\"Done\"        . (\"CheckedBox.xbm\" \"Red\"))
    (\"Conflict\"    . (\"Conflict.xbm\" \"Yellow\")))"
  :group 'mhc
  :type '(repeat
	  :inline t
	  (cons (string :tag "Icon Name")
		(list (string :tag "XBM File Name")
		      (choice (string :tag "Set FG Color")
			      (const :tag "Default FG Color" nil))
		      (choice (string :tag "Set BG Color")
			      (const :tag "Default BG Color" nil))))))

(defcustom mhc-icon-function-alist
  '(("Todo" . mhc-todo-set-as-done)
    ("Done" . mhc-todo-set-as-not-done)
    ("Link" . mhc-browse-x-url))
  "*Alist to define callback function for icons.
Each element should have the form
 (NAME . FUNCTION)
If the icon named NAME is clicked, then FUNCTION is invoked at
icon line."
  :group 'mhc
  :type '(repeat
	  :inline t
	  (cons (string :tag "Icon Name")
		(function :tag "Function"))))

;; internal variable.
(defvar mhc-bm/icon-bmstr-alist nil)
(defvar mhc-bm/icon-function-alist nil)

(defvar mhc-bm-icon-keymap nil)
(if (null mhc-bm-icon-keymap)
    (setq mhc-bm-icon-keymap (make-sparse-keymap)))
(define-key mhc-bm-icon-keymap [mouse-1] 'mhc-bm-icon-call-function)
(define-key mhc-bm-icon-keymap [mouse-2] 'mhc-bm-icon-call-function)

(defun mhc-bm-icon-call-function (event)
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (if (get-text-property (point) 'mhc-bm-icon-function)
	(call-interactively
	 (get-text-property (point) 'mhc-bm-icon-function)))))

(defun mhc-bm/create-rectangle (file)
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((cmp (bitmap-decode-xbm (bitmap-read-xbm-buffer (current-buffer))))
	   (len (length cmp))
	   (i 0)
	   bitmap)
      (while (< i len)
	(setq bitmap (cons (bitmap-compose (aref cmp i)) bitmap))
	(setq i (+ i 1)))
      (nreverse bitmap))))

(defsubst mhc-bm/setup-icons ()
  (let ((alist mhc-bm-icon-alist)
	bmstr)
    (while alist
      ;; Only the first element of the rectangle is used.
      (setq bmstr (car (mhc-bm/create-rectangle
			(expand-file-name (car (cdr (car alist)))
					  mhc-icon-path))))
      (put-text-property 0 (length bmstr)
			 'face 
			 (mhc-face-make-face-from-string
			  (concat "mhc-bm-icon-"
				  (downcase (car (car alist)))
				  "-face")
			  (list nil
				(nth 0 (cdr (cdr (car alist))))
				(nth 1 (cdr (cdr (car alist))))))
			 bmstr)
      (setq mhc-bm/icon-bmstr-alist
	    (cons
	     (cons (downcase (car (car alist)))
		   bmstr)
	     mhc-bm/icon-bmstr-alist))
      (setq alist (cdr alist)))
    (setq mhc-bm/icon-function-alist
	  (mapcar (lambda (pair)
		    (cons (downcase (car pair)) (cdr pair)))
		  mhc-icon-function-alist))))
       
;; Icon interface
(defun mhc-icon-setup ()
  "Initialize MHC icons."
  (interactive)
  (if (interactive-p)
      (setq mhc-bm/icon-bmstr-alist nil))
  (or mhc-bm/icon-bmstr-alist
      (progn
	(message "Initializing MHC icons...")
	(mhc-bm/setup-icons)
	(run-hooks 'mhc-icon-setup-hook)
	(message "Initializing MHC icons...done"))))

(defun mhc-use-icon-p ()
  "Returns t if MHC displays icon."
  (and window-system mhc-use-icon mhc-icon-path))

(defun mhc-icon-exists-p (name)
  "Returns non-nil if icon with NAME exists."
  (cdr (assoc (downcase name) mhc-bm/icon-bmstr-alist)))

(defun mhc-put-icon (icons)
  "Put ICONS on current buffer.
Icon is defined by `mhc-bm-icon-alist'."  
  (let (icon pos func overlay)
    (while icons
      (setq icon (cdr (assoc (downcase (car icons))
			     mhc-bm/icon-bmstr-alist)))
      (setq pos (point))
      (and icon (insert icon))
      (when (setq func (cdr (assoc (downcase (car icons))
				   mhc-bm/icon-function-alist)))
	(put-text-property pos (point)
			   'mhc-bm-icon-function func)
	(put-text-property pos (point) 'local-map mhc-bm-icon-keymap)
	(setq overlay (make-overlay pos (point)))
	(overlay-put overlay 'face (get-text-property 0 'face icon))
	(overlay-put overlay 'mouse-face 'highlight))
      (setq icons (cdr icons)))))

(provide 'mhc-bm)

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

;;; mhc-bm.el ends here
