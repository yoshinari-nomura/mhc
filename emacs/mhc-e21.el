;;; mhc-e21.el -- Emacs 21 stuff for MHC.

;; Author:  Yuuichi Teranishi <teranisi@gohome.org>
;;
;; Created: 2000/11/21
;; Revised: $Date: 2008/03/06 09:40:12 $

(defcustom mhc-e21-icon-alist
  '(("Conflict"   . "Conflict.xpm")
    ("Recurrence" . "Recurrence.xpm")
    ("Private"    . "Private.xpm")
    ("Holiday"    . "Holiday.xpm")
    ("Todo"       . "CheckBox.xpm")
    ("Done"       . "CheckedBox.xpm")
    ("Link"       . "Link.xpm"))
  "*Alist to define icons.
Each element should have the form
 (NAME . ICON-FILE)
It defines icon named NAME created from ICON-FILE.
Example:
  '((\"Holiday\"     . \"Holiday.xpm\")
    (\"Work\"        . \"Business.xpm\")
    (\"Private\"     . \"Private.xpm\")
    (\"Anniversary\" . \"Anniversary.xpm\")
    (\"Birthday\"    . \"Birthday.xpm\")
    (\"Other\"       . \"Other.xpm\")
    (\"Todo\"        . \"CheckBox.xpm\")
    (\"Done\"        . \"CheckedBox.xpm\")
    (\"Conflict\"    . \"Conflict.xpm\"))"
  :group 'mhc
  :type '(repeat
          :inline t
          (cons (string :tag "Icon Name")
                (string :tag "XPM File Name"))))

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

(defvar mhc-e21-icon-keymap nil)
(if (null mhc-e21-icon-keymap)
    (setq mhc-e21-icon-keymap (make-sparse-keymap)))
(define-key mhc-e21-icon-keymap [mouse-1] 'mhc-e21-icon-call-function)
(define-key mhc-e21-icon-keymap [mouse-2] 'mhc-e21-icon-call-function)

(defun mhc-e21-icon-call-function (event)
  (interactive "e")
  (save-excursion
    (mouse-set-point event)
    (when (get-text-property (point) 'mhc-e21-icon-function)
      (call-interactively
       (get-text-property (point) 'mhc-e21-icon-function))
      t)))

;; internal variable.
(defvar mhc-e21/icon-glyph-alist nil)
(defvar mhc-e21/icon-function-alist nil)

(defsubst mhc-e21/setup-icons ()
  (let ((alist mhc-e21-icon-alist)
        name image
        (load-path (cons mhc-icon-path load-path)))
    (setq mhc-e21/icon-glyph-alist nil)
    (while alist
      (setq image (find-image (list (list
                                     :type 'xpm
                                     :file (cdr (car alist))
                                     :ascent 'center))))
      (when image
        (setq mhc-e21/icon-glyph-alist
              (cons
               (cons (downcase (car (car alist))) image)
               mhc-e21/icon-glyph-alist)))
      (setq alist (cdr alist)))
    (setq mhc-e21/icon-function-alist
          (mapcar (lambda (pair)
                    (cons (downcase (car pair)) (cdr pair)))
                  mhc-icon-function-alist))))

;; Icon interface
(defun mhc-icon-setup ()
  "Initialize MHC icons."
  (interactive)
  (if (interactive-p)
      (setq mhc-e21/icon-glyph-alist nil))
  (or mhc-e21/icon-glyph-alist
      (progn
        (message "Initializing MHC icons...")
        (mhc-e21/setup-icons)
        (run-hooks 'mhc-icon-setup-hook)
        (message "Initializing MHC icons...done"))))

(defun mhc-use-icon-p ()
  "Returns t if MHC displays icon."
  (and (display-graphic-p)
       (image-type-available-p 'xpm)
       mhc-use-icon))

(defun mhc-icon-exists-p (name)
  "Returns non-nil if icon with NAME exists."
  (cdr (assoc (downcase name) mhc-e21/icon-glyph-alist)))

(defun mhc-put-icon (icons)
  "Put ICONS on current buffer.
Icon is decided by `mhc-e21-icon-alist'."
  (let (icon pos func props)
    (while icons
      (when (setq icon (cdr (assoc (downcase (car icons))
                                   mhc-e21/icon-glyph-alist)))
        (setq pos (point))
        (insert (make-string (floor (car (image-size icon))) ?  ))
        (setq props (list 'display icon
                          'invisible nil
                          'intangible icon))
        (when (setq func (cdr (assoc (downcase (car icons))
                                     mhc-e21/icon-function-alist)))
          (setq props (nconc props
                             (list
                              'mouse-face 'highlight
                              'mhc-e21-icon-function func
                              'local-map mhc-e21-icon-keymap))))
        (add-text-properties pos (point) props))
      (setq icons (cdr icons)))))

(provide 'mhc-e21)

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

;;; mhc-e21.el ends here
