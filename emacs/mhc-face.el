;;; mhc-face.el

;; Author:  Yoshinari Nomura <nom@quickhack.net>
;;
;; Created: 2000/02/08
;; Revised: $Date: 2004/05/04 13:48:31 $

;;;
;;; Commentay:
;;;

;;;
;;; Code:
;;;

(defvar mhc-symbol-face-alist nil
  "*Alist which is used in setup time to define required faces.
Each element should have the form
  (FACE-SYMBOL . (PARENT FG BG UNDERLINED FONT STIPPLE))
If this variable does't have necessary face definitions for mhc,
mhc will lookup them from mhc-symbol-face-alist-internal instead.
So, this variable doesn't have to cover all the face definitions.")

(defvar mhc-category-face-alist nil
  "*Alist to rule the catgegory-to-face conversion.
Each element should have the form
  (CATEGORY-STRING . (PARENT FG BG UNDERLINED FONT STIPPLE))
mhc will define mhc-summary-category-face-(downcase CATEGORY-STRING)
in setup time.")

(defvar mhc-calendar-hnf-face-alist nil
  "*Alist of HNS faces. Each element should have the form
  (FACE-SYMBOL . (PARENT FG BG UNDERLINED FONT STIPPLE)).
refer to mhc-calendar-hnf-face-alist-internal.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for necessary faces.

(defconst mhc-symbol-face-alist-internal
  '((mhc-calendar-face-default  . (nil nil           nil))
    (mhc-calendar-face-saturday . (nil "blue"        nil))
    (mhc-calendar-face-sunday   . (nil "red"         nil))
    (mhc-calendar-face-duration . (nil nil           "gray"))
    (mhc-calendar-face-cw       . (nil "slate gray"  nil))
    ;;
    (mhc-summary-face-default   . (nil nil           nil))
    (mhc-summary-face-saturday  . (nil "blue"        nil))
    (mhc-summary-face-sunday    . (nil "red"         nil))
    (mhc-summary-face-today     . (nil "black"       "chocolate"))
    (mhc-summary-face-cw        . (nil "slate gray"  nil))
    ;;
    (mhc-summary-face-separator . (nil "gray"        nil))
    (mhc-summary-face-month-separator . (nil "DarkKhaki" nil))
    (mhc-summary-face-time      . (nil "yellowgreen" nil))
    (mhc-summary-face-location  . (nil "black"       "paleturquoise"))
    (mhc-summary-face-conflict  . (nil "white"       "purple"))
    (mhc-summary-face-recurrence . (nil "black"      "green"))
    (mhc-summary-face-secret    . (nil "gray"        nil))
    ;;
    (mhc-minibuf-face-candidate . (nil nil           "yellow"))
    ;;
    (mhc-category-face-holiday  . (nil "red"         nil))))

(defconst mhc-calendar-hnf-face-alist-internal
  '((mhc-calendar-hnf-face-mark . (nil    "MediumSeaGreen" nil))
    (mhc-calendar-hnf-face-newtag  . (italic "red" "paleturquoise"))
    (mhc-calendar-hnf-face-subtag  . (italic "blue" nil))
    (mhc-calendar-hnf-face-cat  . (nil    "DarkGreen" nil))
    (mhc-calendar-hnf-face-new  . (bold   "DarkGreen" nil))
    (mhc-calendar-hnf-face-sub  . (nil   "DarkGreen" nil))
    (mhc-calendar-hnf-face-uri  . (italic "blue" nil))))

(defmacro mhc-face-put (symbol face)
  `(put-text-property 0 (length ,symbol) 'face ,face ,symbol))

(eval-when-compile
  (cond
   ((featurep 'xemacs)
    ;; XEmacs 21.2 (make-face-bold FACE &optional LOCALE TAGS)
    ;; XEmacs 21.1 (make-face-bold FACE &optional LOCALE)
    (defmacro mhc-face/make-face-bold (face)
      `(make-face-bold ,face))
    (defmacro mhc-face/make-face-italic (face)
      `(make-face-italic ,face))
    (defmacro mhc-face/make-face-bold-italic (face)
      `(make-face-bold-italic ,face)))
   (t
    ;; (make-face-bold FACE &optional FRAME NOERROR)
    (defmacro mhc-face/make-face-bold (face)
      `(make-face-bold ,face nil t))
    (defmacro mhc-face/make-face-italic (face)
      `(make-face-italic ,face nil t))
    (defmacro mhc-face/make-face-bold-italic (face)
      `(make-face-bold-italic ,face nil t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make faces from string/symbol

(defun mhc-face-category-to-face (category)
  (if category
      (or (intern-soft (format "mhc-category-face-%s" (downcase category)))
          'default)
    'default))

(defun mhc-face-make-face-from-string (string prop &optional overwrite prefix)
  (let ((symbol-name (concat prefix (if prefix "-") string)))
    (mhc-face-make-face-from-symbol (intern symbol-name) prop overwrite)))

(defun mhc-face-make-face-from-symbol (symbol prop &optional overwrite)
  (let ((parent  (nth 0 prop))
        (fg      (nth 1 prop))
        (bg      (nth 2 prop))
        (uline   (nth 3 prop))
        (font    (nth 4 prop))
        (stipple (nth 5 prop))
        (face    nil))
    (if (and (mhc-facep symbol) (not overwrite))
        symbol
      (setq face (if parent (copy-face parent symbol) (make-face symbol)))
      (if fg      (set-face-foreground  face fg))
      (if bg      (set-face-background  face bg))
      (set-face-underline-p face uline)
      (if font    (set-face-font        face font))
      (if stipple (set-face-stipple     face stipple))
      face)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make faces arrange.

(defvar mhc-face-effect-alist
  ;;             fg      bg        bold  talic  ul
  '((today    . (nil     "gray"    nil   nil    nil))
    (busy     . (nil      nil      t     nil    nil))
    (saturday . ("Blue"   nil      nil   nil    nil))
    (sunday   . ("Red"    nil      nil   nil    nil))))

;; get decolated face from face and effect
;; ex. mhc-summary-face + today -> mhc-summary-face-today
(defun mhc-face-get-effect (face effect)
  (let ((new-face (intern (concat
                           (symbol-name face) "-"
                           (symbol-name effect))))
        effect-list)
    (if (mhc-facep new-face)
        ()
      (copy-face face new-face)
      (if (setq effect-list (cdr (assq effect mhc-face-effect-alist)))
          (let ((fg (nth 0 effect-list))
                (bg (nth 1 effect-list))
                (bl (nth 2 effect-list))
                (it (nth 3 effect-list))
                (ul (nth 4 effect-list)))
            (if fg (set-face-foreground  new-face fg))
            (if bg (set-face-background  new-face bg))
            (if ul (set-face-underline-p new-face t))
            ;;
            (if bl (or (mhc-face/make-face-bold new-face)
                       (and (fboundp 'set-face-bold-p)
                            (set-face-bold-p new-face t))))
            ;;
            (if it (or (mhc-face/make-face-italic new-face)
                       (and (fboundp 'set-face-italic-p)
                            (set-face-italic-p new-face t)))))))
    new-face))
;;
;; (make-face-italic  new-face nil t))))

(defsubst mhc-face-get-today-face (face)
  (mhc-face-get-effect face 'today))

(defsubst mhc-face-get-busy-face (face)
  (mhc-face-get-effect face 'busy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup faces.

(defun mhc-face-setup ()
  (interactive)
  (let ((ow (interactive-p)))
    ;;
    (mhc-face-setup-internal mhc-symbol-face-alist          ow)
    (mhc-face-setup-internal mhc-category-face-alist        ow)
    ;;
    (mhc-face-setup-internal mhc-symbol-face-alist-internal nil)
    ))

(defun mhc-face-setup-internal (alist &optional overwrite)
  (let (lst)
    (while (setq lst (car alist))
      (cond
       ((stringp (car lst))
        (mhc-face-make-face-from-string
         (format "mhc-category-face-%s" (downcase (car lst)))
         (cdr lst)
         overwrite))
       ((symbolp (car lst))
        (mhc-face-make-face-from-symbol
         (car lst)
         (cdr lst)
         overwrite)))
      (setq alist (cdr alist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; facep for emacs 19.28

(cond
 ((fboundp 'find-face)
  (defalias 'mhc-facep 'find-face))
 ((fboundp 'facep)
  (defalias 'mhc-facep 'facep))
 (t
  ;; Introduced in Emacs 19.29.
  (defun mhc-facep (x)
    "Return non-nil if X is a face name or an internal face vector."
    (or (and (fboundp 'internal-facep)
             (let ((fn 'internal-facep))
               ;; Avoid compile warning under old Emacsen.
               (funcall fn x)))
        (and (symbolp x)
             (assq x (and (boundp 'global-face-data)
                          (symbol-value 'global-face-data))))))))

(provide 'mhc-face)

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

;;; mhc-face.el ends here
