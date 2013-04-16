;;; mhc-minibuf.el

;; Author:  Yoshinari Nomura <nom@quickhack.net>
;;
;; Created: 1999/12/10
;; Revised: $Date: 2004/09/08 09:12:10 $

;;;
;;; Commentay:
;;;

;;;
;;; Code:
;;;

(defvar mhc-minibuf-candidate-to-s-func nil)
(defvar mhc-minibuf-candidate-alist     nil)
(defvar mhc-minibuf-candidate-offset    0)
(defvar mhc-minibuf-candidate-overlay   nil)
(defvar mhc-minibuf-candidate-buffer    nil)
(defvar mhc-minibuf-candidate-delimiter nil)

;; (make-variable-buffer-local 'mhc-minibuf-candidate-to-s-func)
;; (make-variable-buffer-local 'mhc-minibuf-candidate-alist)
;; (make-variable-buffer-local 'mhc-minibuf-candidate-offset)
;; (make-variable-buffer-local 'mhc-minibuf-candidate-overlay)
;; (make-variable-buffer-local 'mhc-minibuf-candidate-buffer)

(defun mhc-minibuf-read (&optional prompt default
                                   buffer cand offset to-s delimiter)
  (if mhc-minibuf-candidate-overlay
      (delete-overlay mhc-minibuf-candidate-overlay))
  (setq mhc-minibuf-candidate-buffer    buffer
        mhc-minibuf-candidate-alist     cand
        mhc-minibuf-candidate-offset    (or offset 0)
        mhc-minibuf-candidate-to-s-func to-s
        mhc-minibuf-candidate-delimiter delimiter)
  (if cand
      (progn
        (setq mhc-minibuf-candidate-overlay
              (make-overlay
               (mhc-minibuf-candidate-nth-begin)
               (mhc-minibuf-candidate-nth-end)
               buffer))
        (overlay-put mhc-minibuf-candidate-overlay 'face
                     'mhc-minibuf-face-candidate)
        (mhc-minibuf-move-candidate 0 t t)))
  (read-from-minibuffer
   prompt
   (cond
    (default default)
    ((and to-s (mhc-minibuf-candidate-nth-obj)
          (funcall to-s (mhc-minibuf-candidate-nth-obj))))
    (t ""))
   mhc-minibuf-map))

;; access methods to candidate-alist
;;
;; candidate-alist is like:
;;    ((score (begin . end) obj) ...)
;;

(defun mhc-minibuf/get-nth-candidate  (&optional alist n)
  (nth (or n mhc-minibuf-candidate-offset)
       (or alist mhc-minibuf-candidate-alist)))

(defun mhc-minibuf-candidate-nth-score (&optional alist n)
  (let ((candidate (mhc-minibuf/get-nth-candidate alist n)))
    (if candidate (mhc-guess-get-score candidate))))

(defun mhc-minibuf-candidate-nth-begin (&optional alist n)
  (let ((candidate (mhc-minibuf/get-nth-candidate alist n)))
    (if candidate (mhc-guess-get-begin candidate))))

(defun mhc-minibuf-candidate-nth-end (&optional alist n)
  (let ((candidate (mhc-minibuf/get-nth-candidate alist n)))
    (if candidate (mhc-guess-get-end candidate))))

(defun mhc-minibuf-candidate-nth-obj (&optional alist n)
  (let ((candidate (mhc-minibuf/get-nth-candidate alist n)))
    (if candidate (cons
                   (mhc-guess-get-date-or-time candidate)
                   (mhc-guess-get-date-or-time-end candidate)))))

;;
;; move candidate by score.
;;
(defun mhc-minibuf-candidate-inc-offset2 (&optional n)
  (let ((len (length mhc-minibuf-candidate-alist)))
    (if (< 0 len)
        (setq mhc-minibuf-candidate-offset
              (%
               (+ len (% (+ mhc-minibuf-candidate-offset (or n 1)) len))
               len)))))

;;
;; move candidate by position.
;; xxx: offset is 1 or -1 only.
;;
(defun mhc-minibuf-candidate-inc-offset (&optional n)
  (let* ((len (length mhc-minibuf-candidate-alist))
         (cur (mhc-minibuf-candidate-nth-begin))
         (max cur)
         (min cur)
         (max-i mhc-minibuf-candidate-offset)
         (min-i mhc-minibuf-candidate-offset)
         (i 0)
         nxt prv ptr prv-i nxt-i)
    (while (< i len)
      (setq ptr (mhc-minibuf-candidate-nth-begin
                 mhc-minibuf-candidate-alist i))
      (if (< max ptr) (setq max ptr max-i i))
      (if (< ptr min) (setq min ptr min-i i))
      (if (and (< cur ptr) (or (null nxt) (< ptr nxt)))
          (setq nxt ptr nxt-i i))
      (if (and (< ptr cur) (or (null prv) (< prv ptr)))
          (setq prv ptr prv-i i))
      (setq i (1+ i)))
    (if (< 0 n)
        (setq mhc-minibuf-candidate-offset (if nxt-i nxt-i min-i))
      (setq mhc-minibuf-candidate-offset (if prv-i prv-i max-i)))))


(defun mhc-minibuf-candidate-set-offset (n)
  (setq mhc-minibuf-candidate-offset n))

;;
;; keybind
;;

(defvar mhc-minibuf-map nil)
;; (setq mhc-minibuf-map nil)

(if mhc-minibuf-map
    ()
  (setq mhc-minibuf-map (copy-keymap minibuffer-local-map))
  (define-key mhc-minibuf-map "\C-c?" 'mhc-minibuf-insert-calendar)
  (define-key mhc-minibuf-map "\C-n"  'mhc-minibuf-next-candidate)
  (define-key mhc-minibuf-map "\C-p"  'mhc-minibuf-prev-candidate)
  (define-key mhc-minibuf-map "\C-v"  'scroll-other-window)
  (define-key mhc-minibuf-map "\M-v"  'scroll-other-window-down))

;;
;; minibuffer functions
;;

;; (defun mhc-minibuf-delete-word ()
;;   (delete-region
;;    (save-excursion
;;      (while (and (not (bobp))
;;               (string-match "[0-9:/-]"
;;                             (buffer-substring
;;                              (1- (point)) (point))))
;;        (forward-char -1))
;;      (point))
;;    (point)))

(defun mhc-minibuf-delete-word (&optional delimiter)
  (delete-region
   (save-excursion
     (while (and (not (bobp))
                 (string-match (or delimiter "[0-9:/-]")
                               (buffer-substring
                                (1- (point)) (point))))
       (forward-char -1))
     (point))
   (point)))



(defun mhc-minibuf-move-candidate (offset &optional absolute non-minibuf)
  (if (not mhc-minibuf-candidate-alist)
      ()
    (if absolute
        (mhc-minibuf-candidate-set-offset offset)
      (mhc-minibuf-candidate-inc-offset offset))
    ;; (y-or-n-p (format "%d" mhc-minibuf-candidate-offset))
    (let* ((b    (mhc-minibuf-candidate-nth-begin))
           (e    (mhc-minibuf-candidate-nth-end))
           (obj  (mhc-minibuf-candidate-nth-obj))
           (str  (if (and mhc-minibuf-candidate-to-s-func obj)
                     (funcall mhc-minibuf-candidate-to-s-func obj) "")))
      (if (not (and mhc-minibuf-candidate-overlay b))
          ()
        (move-overlay mhc-minibuf-candidate-overlay b e)
        (if (not non-minibuf)
            (pop-to-buffer mhc-minibuf-candidate-buffer))
        (goto-char b)
        (if (not (pos-visible-in-window-p b))
            (recenter))
        (if (not non-minibuf)
            (pop-to-buffer (window-buffer (minibuffer-window))))
        ;; in minibuffer
        (if non-minibuf
            ()
          ;; (if (string-match "-" str)
          ;; (delete-region (point-min) (point-max))
          ;; (mhc-minibuf-delete-word))
          (mhc-minibuf-delete-word
           mhc-minibuf-candidate-delimiter)
          (insert str))))))

(defun mhc-minibuf-next-candidate ()
  (interactive)
  (mhc-minibuf-move-candidate 1))

(defun mhc-minibuf-prev-candidate ()
  (interactive)
  (mhc-minibuf-move-candidate -1))

;;
;; input functions for mhc.
;;

(defun mhc-minibuf/date-to-string (date-cons)
  (let ((date (car date-cons))
        (date2 (cdr date-cons)))
    (concat
     (mhc-date-format date "%04d/%02d/%02d" yy mm dd)
     (if date2
         (mhc-date-format date2 "-%04d/%02d/%02d" yy mm dd)
       ""))))

(defun mhc-minibuf/time-to-string (time-cons)
  (let ((time (car time-cons))
        (time2 (cdr time-cons)))
    (if time2
        (concat
         (mhc-time-to-string time) "-" (mhc-time-to-string time2))
      (mhc-time-to-string time))))

(defun mhc-minibuf/location-to-string (location-cons)
  (let ((loc (car location-cons))
        (loc2 (cdr location-cons)))
    (if loc2
        (concat
         (format "%s" loc) "-" (format "%s" loc2))
      (format "%s" loc))))

(defun mhc-input-day (&optional prompt default candidate)
  (interactive)
  (let (str-list date ret (error t) str)
    (while error
      (setq str
            (mhc-minibuf-read
             (concat (or prompt "") "(yyyy/mm/dd): ")
             (if candidate
                 nil
               (cond
                ((and (stringp default)
                      (mhc-date-new-from-string default t))
                 default)
                ((mhc-date-p default)
                 (mhc-date-format default
                                  "%04d/%02d/%02d" yy mm dd))
                ((listp default)
                 (mapconcat
                  (lambda (date)
                    (mhc-date-format date
                                     "%04d/%02d/%02d" yy mm dd))
                  default
                  " "))
                (t
                 nil)))
             (current-buffer)
             candidate
             0
             (function mhc-minibuf/date-to-string))
            str-list (mhc-misc-split str)
            ret       nil
            error    nil)
      (while (car str-list)
        (cond
         ((= 2 (length (mhc-misc-split (car str-list) "-")))
          (let* ((duration (mhc-misc-split (car str-list) "-"))
                 (b (mhc-date-new-from-string2 (nth 0 duration) nil t))
                 (e (mhc-date-new-from-string2 (nth 1 duration) b t)))
            (if (and b e (mhc-date< b e))
                (progn
                  (setq date b)
                  (while (mhc-date<= date e)
                    (if (not (member date ret))
                        (setq ret (cons date ret))
                      (setq error t))
                    (setq date (mhc-date++ date))))
              (setq error t))))
         ((string= (car str-list) "")
          ())
         ((setq date (mhc-date-new-from-string2 (car str-list) date t))
          (if (not (member date ret))
              (setq ret (cons date ret))
            (setq error t)))
         ((string= (car str-list) "none")
          ())
         (t
          (setq error t)))
        (setq str-list (cdr str-list)))
      (if error (beep)))
    (mhc-calendar-input-exit)
    (mhc-date-sort ret)))

(defun mhc-input-time (&optional prompt default candidate)
  (interactive)
  (let (str time-b time-e)
    (catch 'ok
      (while t
        (setq str (mhc-minibuf-read
                   (concat (or prompt "") "(HH:MM-HH:MM or none) ")
                   (if candidate
                       nil
                     (if default
                         (if (stringp default) default
                           (mhc-minibuf/time-to-string default) "")))
                   (current-buffer)
                   candidate
                   0
                   (function mhc-minibuf/time-to-string)))
        (cond
         ((and (string-match
                "^\\([0-9]+:[0-9]+\\)\\(-\\([0-9]+:[0-9]+\\)\\)?$" str)
               (setq time-b (mhc-time-new-from-string
                             (substring str (match-beginning 1) (match-end 1))
                             t mhc-input-time-regex)))
          (if (not (match-beginning 3)) (throw 'ok (list time-b nil)))
          (if (and (setq time-e
                         (mhc-time-new-from-string
                          (substring str (match-beginning 3) (match-end 3))
                          t mhc-input-time-regex))
                   (mhc-time<= time-b time-e))
              (throw 'ok (list time-b time-e))))
         ((string= "" str)
          (throw 'ok (list nil nil))))
        (beep)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; input x-sc- schedule data from minibuffer.


(defvar mhc-month-hist nil)

(defun mhc-input-month (prompt &optional default)
  (let ((ret nil)
        (month-str (mhc-date-format (or default (mhc-date-now)) "%04d/%02d" yy mm)))
    (while (null ret)
      (setq month-str
            (read-from-minibuffer
             (concat prompt "(yyyy/mm) : ") month-str nil nil 'mhc-month-hist))
      (if (string-match "\\([0-9]+\\)/\\([0-9]+\\)" month-str)
          (setq ret (mhc-date-new
                     (string-to-number (match-string 1 month-str))
                     (string-to-number (match-string 2 month-str))
                     1 t))))
    ret))

(defconst mhc-input-time-regex "^\\([0-9]+\\):\\([0-9]+\\)$")

(defvar mhc-subject-hist nil)

(defun mhc-input-subject (&optional prompt default)
  (interactive)
  (read-from-minibuffer  (or prompt "Subject: ")
                         (or default "")
                         nil nil 'mhc-subject-hist))

(defvar mhc-location-hist nil)

;; (defun mhc-input-location (&optional prompt default)
;;   (interactive)
;;   (read-from-minibuffer  (or prompt "Location: ")
;;                       (or default "")
;;                       nil nil 'mhc-location-hist))

(defun mhc-input-location (&optional prompt default)
  (mhc-minibuf-read "Location: "
                    default
                    (current-buffer)
                    (mhc-guess-location)
                    0
                    (function mhc-minibuf/location-to-string)
                    "[^ ]"))

(defvar mhc-category-hist nil)

(if (fboundp 'completing-read-multiple)
    (defun mhc-input-category (&optional prompt default)
      (interactive)
      (let ((completion-ignore-case t)
            (table (nconc (delete '("Todo")
                                  (delete '("Done")
                                          (mapcar (lambda (x) (list (car x)))
                                                  mhc-category-face-alist)))
                          (list '("Todo") '("Done")))))
        (completing-read-multiple (or prompt "Category: ")  ;PROMPT
                                  table
                                  nil                 ;PREDICATE
                                  nil                 ;REQUIRE-MATCH
                                  default             ;INITIAL-INPUT
                                  'mhc-category-hist  ;HIST
                                  )))

  (defun mhc-input-category (&optional prompt default)
    (interactive)
    (let (in)
      (and default
           (listp default)
           (setq default (mapconcat 'identity default " ")))
      (if (string= "" (setq in (read-from-minibuffer
                                (or prompt "Category: ")
                                (or default "")
                                nil nil 'mhc-category-hist)))
          nil
        (mhc-misc-split in)))))


(defvar mhc-recurrence-tag-hist nil)

(defun mhc-input-recurrence-tag (&optional prompt default)
  (interactive)
  (read-from-minibuffer  (or prompt "Recurrence Tag: ")
                         (or default "")
                         nil nil 'mhc-recurrence-tag-hist))


(defvar mhc-alarm-hist nil)

(defun mhc-input-alarm (&optional prompt default)
  (interactive)
  (read-from-minibuffer  (or prompt "Alarm: ")
                         (or default mhc-default-alarm)
                         nil nil 'mhc-alarm-hist))

(provide 'mhc-minibuf)

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

;;; mhc-minibuf.el ends here
