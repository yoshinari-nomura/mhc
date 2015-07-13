;;; calfw-mhc.el --- calfw calendar view for mhc

;; Author: Yoshinari Nomura <nom@quickhack.net>

;;; Commentary:

;; setting example:
;;
;; (require 'calfw)
;; (require 'calfw-mhc)
;; (require 'calfw-org)
;;
;; (defun open-calendar ()
;;  (interactive)
;;  (cfw:open-calendar-buffer
;;   :view 'month
;;   :contents-sources
;;   (list
;;    (cfw:org-create-source "Seagreen4")
;;    (cfw:mhc-create-source "all"      "black"  "!(Holiday || Birthday)")
;;    (cfw:mhc-create-source "birthday" "yellow" "Birthday")
;;    (cfw:mhc-create-source "holiday"  "red"    "Holiday"))))

;;; Code:

(require 'mhc)
(require 'calfw)

(defvar cfw:mhc-text-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] 'cfw:mhc-open-article)
    (define-key map (kbd "<return>") 'cfw:mhc-open-article)
    map)
  "key map on the calendar item text.")

(defvar cfw:mhc-schedule-map
  (cfw:define-keymap
   '(
     ("q" . cfw:mhc-close-article)
     ("SPC" . cfw:mhc-open-article)
     ))
  "Key map for the mhc calendar mode.")

(defun cfw:mhc-schedule-cache-clear ())

(defun cfw:to-mhc-date (date)
  (mhc-date-new (nth 2 date) (nth 0 date) (nth 1 date)))

(defun cfw:mhc-to-calfw-date (mhc-date)
  (mhc-day-let mhc-date
    (list month day-of-month year)))

(defun cfw:mhc-make-one-day-entry (day-info &optional category-predicate)
  (cons
   (cfw:mhc-to-calfw-date
    (mhc-day-date day-info))
   (delq nil
         (mapcar
          (lambda (sch)
            (if (funcall category-predicate sch)
                (cfw:mhc-make-summary-string sch) nil))
          (mhc-day-schedules day-info)))))

(defun blank-p (s)
  (not (and s (not (string= s "")))))

;;
;; Although mhc has its own formatting functions for this purpose,
;; they seems to require some modification to get along with calfw.
;; I'm in the mood for fixing the functions in mhc for the
;; first time almost in a decade :-)
;;
(defun cfw:mhc-make-summary-string (schedule)
  (let ((line
         (format "%s %s %s"
                 (mhc-schedule-time-as-string schedule)
                 (mhc-schedule-subject-as-string schedule)
                 (if (blank-p (mhc-schedule-location schedule))
                     ""
                   (format "[%s]" (mhc-schedule-location schedule))))))
    (propertize
     line
     'keymap cfw:mhc-text-keymap
     'mhc-schedule schedule)))

(defun cfw:mhc-schedule-period-to-calendar (begin end &optional category)
  (let ((category-predicate (mhc-expr-compile category)))
    (mapcar
     (lambda (day-info)
       (cfw:mhc-make-one-day-entry day-info category-predicate))
     (mhc-db-scan
      (cfw:to-mhc-date begin)
      (cfw:to-mhc-date end)
      'nosort))))

(defun cfw:mhc-create-source (name &optional color category)
  (lexical-let ((category category))
    (make-cfw:source
     :name (concat "mhc:" name)
     :color (or color "SteelBlue")
     :update 'cfw:mhc-schedule-cache-clear
     :data (lambda (begin end) (cfw:mhc-schedule-period-to-calendar begin end category)))))

(defun cfw:mhc-close-article ()
  (interactive)
  (mhc-window-pop)
  (kill-buffer))

(defun cfw:mhc-open-article ()
  (interactive)
  (mhc-window-push)
  (let ((schedule (get-text-property (point) 'mhc-schedule)))
    (if schedule
        (cfw:details-popup
         (with-temp-buffer
           (mhc-insert-file-contents-as-coding-system
            mhc-default-coding-system
            (mhc-record-name (mhc-schedule-record schedule)))
           (mhc-calendar/view-file-decode-header)
           (buffer-string)
           ))
      (message "mhc schedule not found"))))

(defun cfw:open-mhc-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :view 'month
   :contents-sources
   (list
    (cfw:mhc-create-source "all"      "black"  "!(Holiday | Birthday)")
    (cfw:mhc-create-source "birthday" "brown"  "Birthday")
    (cfw:mhc-create-source "holiday"  "red"    "Holiday"))))

(provide 'mhc-calfw)

;;; mhc-calfw.el ends here
