;;; mhc-misc.el -- miscellaneous functions for mhc.

;; Author:  Yoshinari Nomura <nom@quickhack.net>
;;
;; Created: 1997/10/12
;; Revised: $Date: 2002/12/01 03:55:06 $

;;;
;;; Commentay:
;;;

;;;
;;; Code:
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string

(defun mhc-misc-sub (str regex replace)
  (if (and (stringp str) (string-match regex str))
      (concat (substring str 0 (match-beginning 0))
	      replace
	      (substring str (match-end 0)))
    str))

(defun mhc-misc-gsub (str regex replace)
  (if (and (stringp str) (string-match regex str))
      (concat (substring str 0 (match-beginning 0))
	      replace
	      (mhc-misc-gsub (substring str (match-end 0)) regex replace))
    str))

(defun mhc-misc-split (str &optional sep)
  (let ((ret ()))
    (while (string-match (or sep "[\t ]+") str)
      (setq ret (cons (substring str 0 (match-beginning 0)) ret))
      (setq str (substring str (match-end 0))))
    (nreverse (cons str ret))))

(defun mhc-misc-strip (str)
  (mhc-misc-sub 
   (mhc-misc-sub str "^[\t ]+" "") "[\t ]+$" ""))

(defun mhc-misc-substring-to-int (str pos)
  (cond
   ((stringp str)
    (string-to-int
     (substring str (match-beginning pos) (match-end pos))))
   (t
    (string-to-int
     (buffer-substring (match-beginning pos) (match-end pos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file & path

(defun mhc-misc-get-new-path (dir)
  "Return name for new schedule file on DIR."
  (let (dirent (max 0) (num nil))
    (mhc-file-make-directory dir)
    (setq dirent (directory-files dir nil nil t))
    (while dirent
      (or (string-match "[^0-9]" (car dirent))
	  (if (< max (setq num (string-to-number (car dirent))))
	      (setq max num)))
      (setq dirent (cdr dirent)))
    (expand-file-name (number-to-string (1+ max)) dir)))

;;
;; touch directory and files.
;;

(defvar mhc-mtime-file ".mhc-mtime")

(defun mhc-misc-get-mtime (obj)
  (let ((mtime-file (expand-file-name mhc-mtime-file obj)))
    (cond
     ((not (stringp obj))
      nil)
     ((file-exists-p mtime-file)
      (nth 5 (file-attributes mtime-file)))
     ((file-exists-p obj)
      (nth 5 (file-attributes obj)))
     (t
      nil))))

(defun mhc-misc-touch-directory (dir)
  (let ((mtime-file (expand-file-name mhc-mtime-file dir)))
    (if (file-writable-p mtime-file)
	;; (write-region (point-min) (point-min) mtime-file nil 'silence))
	(write-region 1 2 mtime-file nil 'silence))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rectangle
;;
;; Does (current-column) count visible character only?
;;   emacs  19.34, 20.4, 20.5 -- yes
;;   emacs  19.28             -- no
;;  xemacs                    -- no
;;
(defvar mhc-misc-column-count-visible-only
  (and (not (featurep 'xemacs)) (string< "19.3" emacs-version)))

(defun mhc-misc-move-to-column (column)
  "Move point to column COLUMN rigidly in the current line, considering 
   invisible charracters."
  (if mhc-misc-column-count-visible-only
      ()
    (beginning-of-line)
    (let* ((bol (point))
	   (vis (if (get-char-property bol 'invisible)
		    (next-single-property-change bol 'invisible)
		  bol)))
      (setq column (+ column (- vis bol)))))
  (if (< column (move-to-column column t))
      (progn (delete-char -1) (insert ?\ ))))

(defun mhc-misc-current-column ()
  "Return current column in a visible field."
  (if mhc-misc-column-count-visible-only
      (current-column)
    (let* ((bol (save-excursion (beginning-of-line) (point)))
	   (vis (if (get-char-property bol 'invisible)
		    (next-single-property-change bol 'invisible)
		  bol)))
      (- (current-column) (- vis bol)))))

(defun mhc-misc-insert-rectangle (rectangle)
  (let ((lines rectangle)
        (insertcolumn (mhc-misc-current-column))
        (first t))
    ;; (push-mark)
    (while lines
      (or first
          (progn
	    (forward-line 1)
	    (or (bolp) (insert ?\n))
	    (mhc-misc-move-to-column insertcolumn)))
      (setq first nil)
      (if (looking-at "[^\r\n]+")
	  (delete-region (point) (match-end 0)))
      (insert (car lines))
      (setq lines (cdr lines)))))

(defun mhc-misc-get-width ()
  (let ((dw (* mhc-calendar-width 2))
	(ww (window-width))
	(fw (frame-width)))
    (cond
     ((> ww dw) ww)
     ((and (< (* ww 2) fw)
	   (> (* ww 2) dw)) (* ww 2))
     ((> fw dw) fw)
     (t dw))))

;; read-passwd

(defun mhc-misc-read-passwd (prompt)
  (let ((inhibit-input-event-recording t))
    (if (fboundp 'read-passwd)
	(condition-case nil
	    (read-passwd prompt)
	  ;; If read-passwd causes an error, let's return "" so that
	  ;; the password process will safely fail.
	  (error ""))
      (let ((pass "")
	    (c 0)
	    (echo-keystrokes 0)
	    (ociea cursor-in-echo-area))
	(condition-case nil
	    (progn
	      (setq cursor-in-echo-area 1)
	      (while (and (/= c ?\r) (/= c ?\n) (/= c ?\e) (/= c 7)) ;; ^G
		(message "%s%s"
			 prompt
			 (make-string (length pass) ?.))
		(setq c (read-char-exclusive))
		(cond
		 ((char-equal c ?\C-u)
		  (setq pass ""))
		 ((or (char-equal c ?\b) (char-equal c ?\177))  ;; BS DELL
		  ;; delete one character in the end
		  (if (not (equal pass ""))
		      (setq pass (substring pass 0 -1))))
		 ((< c 32) ()) ;; control, just ignore
		 (t
		  (setq pass (concat pass (char-to-string c))))))
	      (setq cursor-in-echo-area -1))
	  (quit
	   (setq cursor-in-echo-area ociea)
	   (signal 'quit nil))
	  (error
	   ;; Probably not happen. Just align to the code above.
	   (setq pass "")))
	(setq cursor-in-echo-area ociea)
	(message "")
	(sit-for 0)
	pass))))

(provide 'mhc-misc)

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

;;; mhc-misc.el ends here
