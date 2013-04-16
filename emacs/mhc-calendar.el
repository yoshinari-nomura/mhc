;;; -*- emacs-lisp -*-
;; mhc-calendar.el -- MHC Mini calendar
;;
;; Author:  Hideyuki SHIRAI <shirai@quickhack.net>
;;          MIYOSHI Masanori <miyoshi@quickhack.net>
;;
;; Created: 05/12/2000
;; Reviesd: $Date: 2008/03/06 09:40:12 $

;;; Configration Variables:

(defcustom mhc-calendar-language 'english
  "*Language of the calendar."
  :group 'mhc
  :type '(choice (const :tag "English" english)
                 (const :tag "Japanese" japanese)))

(defcustom mhc-calendar-separator ?|
  "*Character of the separator between Summary and Vertical calendar."
  :group 'mhc
  :type 'character)

(defcustom mhc-calendar-use-cw nil
  "*Displayed style of `Calendar week number'."
  :group 'mhc
  :type '(choice (const :tag "No" nil)
                 (const :tag "Month" month)
                 (const :tag "Week" week)))

(defcustom mhc-calendar-cw-indicator
  (if (eq mhc-calendar-language 'japanese) "週" "Cw")
  "*Indicator of Calendar week."
  :group 'mhc
  :type 'string)

(defcustom mhc-calendar-day-strings
  (if (eq mhc-calendar-language 'japanese)
      '["日" "月" "火" "水" "木" "金" "土"]
    '["Su" "Mo" "Tu" "We" "Th" "Fr" "Sa"])
  "*Vector of \"day of week\" for 3-month calendar header."
  :group 'mhc
  :type '(list string string string string string string string))

(defcustom mhc-calendar-header-function
  (if (eq mhc-calendar-language 'japanese)
      'mhc-calendar-make-header-ja
    'mhc-calendar-make-header)
  "*Function of \"make calendar header\" for 3-month calendar.
Assigned function must have one option \"date\"
and must return string like \"   December 2000\"."
  :group 'mhc
  :type '(radio
          (function-item :tag "English" mhc-calendar-make-header)
          (function-item :tag "Japanese" mhc-calendar-make-header-ja)
          (function :tag "Other")))

(defvar mhc-calendar-inserter-date-list
  '(((yy mm02 dd02) . "-")
    ((yy "/" mm02 "/" dd02) . "-")
    ((mm02 "/" dd02 "/" yy "(" ww-string ")") . "-")
    ((yy "." mm02 "." dd02 "(" ww-string ")") . " - ")
    ((yy "-" mm02 "-" dd02 "(" ww-string ")") . " - ")
    ((dd02 "-" mm-string "-" yy "(" ww-string ")") . " - ")
    ((ww-string ", " dd02 " " mm-string " " yy) . " - ")
    ((yy "年" mm2 "月" dd2 "日(" ww-japanese ")") . ("〜" " - "))
    ((mm "月" dd2 "日(" ww-japanese ")") . ("〜" " - "))
    ((nengo mm2 "月" dd2 "日(" ww-japanese ")") . ("〜" " - ")))
  "*List of date inserters.
Each cell has a cons cell, car slot has a format of 'date modifier funcitons'
and cdr slot has a which 'concatenate string' or its list for the duration.
E.g., if date equal \"Mon, 01 May 2000\", symbol return a string described below,

yy               => \"2000\"
nengo            => \"平成12年\"
mm               => \"7\"
mm2              => \" 7\"
mm02             => \"07\"
mm-string        => \"Jul\"
mm-string-long   => \"July\"
dd              =>  \"1\"
dd2              => \" 1\"
dd02             => \"01\"
ww               => \"6\"
ww-string        => \"Sat\"
ww-string-long   => \"Saturday\"
ww-japanese      => \"土\"
ww-japanese-long => \"土曜日\"
")

(defcustom mhc-calendar-mode-hook nil
  "*Hook called in mhc-calendar-mode."
  :group 'mhc
  :type 'hook)

(defcustom mhc-calendar-create-buffer-hook nil
  "*Hook called in mhc-calendar-create-buffer."
  :group 'mhc
  :type 'hook)

(defcustom mhc-calendar-start-column 2
  "*Size of left margin."
  :group 'mhc
  :type 'integer)

(defcustom mhc-calendar-height
  (cond
   ((and (featurep 'xemacs) window-system) 12)
   ((and (not (featurep 'xemacs)) (>= emacs-major-version 21)) 10)
   (t 9))
  "*Height of next month start column (greater or equal 9)."
  :group 'mhc
  :type 'integer)

(defcustom mhc-calendar-height-offset
  (cond
   ((and (featurep 'xemacs) window-system) 4)
   ((and (not (featurep 'xemacs)) (>= emacs-major-version 21)) 3)
   (t 1))
  "*Offset of window height."
  :group 'mhc
  :type 'integer)

(defcustom mhc-calendar-view-summary nil
  "*View day's summary if *non-nil*."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-calendar-link-hnf nil
  "*Support HNF(Hyper Nikki File) mode if *non-nil*."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-calendar-use-mouse-highlight t
  "*Highlight mouse pointer."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-calendar-use-help-echo t
  "*Display schedule within help-echo."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-calendar-use-duration-show (if window-system 'mixed 'modeline)
  "*Show 'duration' mode."
  :group 'mhc
  :type '(choice
          (const :tag "none" nil)
          (const :tag "modeline" modeline)
          (const :tag "face" face)
          (const :tag "mixed" mixed)))

(defcustom mhc-calendar-view-file-hook nil
  "*Hook called in mhc-calendar-view-file."
  :group 'mhc
  :type 'hook)

;; internal variables. Don't modify.
(defvar mhc-calendar/buffer "*mhc-calendar*")
(defvar mhc-calendar-date nil)
(defvar mhc-calendar-view-date nil)
(defvar mhc-calendar-mode-map nil)
(defvar mhc-calendar-mode-menu-spec nil)
(defvar mhc-calendar/week-header nil)
(defvar mhc-calendar/separator-str nil)

(defvar mhc-calendar/inserter-call-buffer nil)
(defvar mhc-calendar/inserter-type nil)
(defvar mhc-calendar/inserter-for-minibuffer '(((yy "/" mm02 "/" dd02) . "-")))
(defvar mhc-calendar/inserter-for-draft '(((yy mm02 dd02) . "-")))
(defvar mhc-calendar/mark-date nil)

;; mhc-calendar functions
;; macros
(defmacro mhc-calendar-p ()
  `(eq major-mode 'mhc-calendar-mode))

(defmacro mhc-calendar/in-date-p () ;; return 'date from 01/01/1970'
  `(get-text-property (point) 'mhc-calendar/date-prop))

(defmacro mhc-calendar/in-summary-p () ;; return 'schedule filename'
  `(or (get-text-property (point) 'mhc-calendar/summary-prop)
       (save-excursion
         (beginning-of-line)
         (get-text-property (point) 'mhc-calendar/summary-prop))))

(defmacro mhc-calendar/in-summary-hnf-p () ;; return 'title count'
  `(or (get-text-property (point) 'mhc-calendar/summary-hnf-prop)
       (save-excursion
         (beginning-of-line)
         (get-text-property (point) 'mhc-calendar/summary-hnf-prop))))

(defmacro mhc-calendar/cw-week ()
  `(and (or (eq mhc-calendar-use-cw 'week)
            (eq mhc-calendar-use-cw t))
        (eq mhc-start-day-of-week 1)))

(defcustom mhc-calendar-next-offset (if (mhc-calendar/cw-week) 27 23)
  "*Offset of next month start column (greater or equal 23)."
  :group 'mhc
  :type 'integer)

(defvar mhc-calendar-width (if (mhc-calendar/cw-week) 28 24))

(defmacro mhc-calendar/cw-string (cw)
  `(let (ret)
     (if (stringp ,cw)
         (setq ret ,cw)
       (setq ret (format "%2d." ,cw)))
     (mhc-face-put ret 'mhc-calendar-face-cw)
     ret))

(defmacro mhc-calendar/get-date-colnum (col)
  `(cond
    ((< ,col (+ mhc-calendar-next-offset mhc-calendar-start-column)) -1)
    ((< ,col (+ (* mhc-calendar-next-offset 2) mhc-calendar-start-column)) 0)
    (t 1)))

(defmacro mhc-calendar/buffer-substring-to-num (pos)
  `(string-to-number
    (buffer-substring (match-beginning ,pos) (match-end ,pos))))

;; Avoid warning of byte-compiler.
(eval-when-compile
  (defvar yy)
  (defvar mm)
  (defvar dd)
  (defvar ww)
  (defvar hnf-diary-dir)
  (defvar hnf-diary-year-directory-flag)
  (defvar view-exit-action)
  (defvar mhc-calendar-mode-menu))

(eval-and-compile
  (autoload 'easy-menu-add "easymenu")
  (autoload 'hnf-mode "hnf-mode"))

;; Compatibilities between emacsen
(if (fboundp 'text-property-any)
    (defsubst mhc-calendar/tp-any (beg end prop value)
      (text-property-any beg end prop value))
  (defsubst mhc-calendar/tp-any (beg end prop value)
    (while (and beg (< beg end)
                (not (eq value (get-text-property beg prop))))
      (setq beg (next-single-property-change beg prop nil end)))
    (if (eq beg end) nil beg)))

(if (fboundp 'event-buffer)
    (defalias 'mhc-calendar/event-buffer 'event-buffer)
  (defun mhc-calendar/event-buffer (event)
    (window-buffer (posn-window (event-start event)))))

(if (fboundp 'event-point)
    (defalias 'mhc-calendar/event-point 'event-point)
  (defun mhc-calendar/event-point (event)
    (posn-point (event-start event))))

;; map/menu
(unless mhc-calendar-mode-map
  (setq mhc-calendar-mode-map (make-sparse-keymap))
  (define-key mhc-calendar-mode-map "."    'mhc-calendar-goto-today)
  (define-key mhc-calendar-mode-map "g"    'mhc-calendar-goto-month)
  (define-key mhc-calendar-mode-map "r"    'mhc-calendar-rescan)
  (define-key mhc-calendar-mode-map "R"    'mhc-reset)
  (define-key mhc-calendar-mode-map "="    'mhc-calendar-get-day)
  (define-key mhc-calendar-mode-map " "    'mhc-calendar-get-day-insert)
  (define-key mhc-calendar-mode-map "\C-m" 'mhc-calendar-get-day-insert-quit)
  (define-key mhc-calendar-mode-map "-"    'mhc-calendar-count-days-region)
  (define-key mhc-calendar-mode-map "s"    'mhc-calendar-scan)
  (define-key mhc-calendar-mode-map "E"    'mhc-calendar-edit)
  (define-key mhc-calendar-mode-map "M"    'mhc-calendar-modify)
  (define-key mhc-calendar-mode-map "D"    'mhc-calendar-delete)
  (define-key mhc-calendar-mode-map "H"    'mhc-calendar-hnf-edit)
  (define-key mhc-calendar-mode-map "v"    'mhc-calendar-goto-view)
  (define-key mhc-calendar-mode-map "h"    'mhc-calendar-goto-home)
  (define-key mhc-calendar-mode-map "f"    'mhc-calendar-next-day)
  (define-key mhc-calendar-mode-map "b"    'mhc-calendar-prev-day)
  (define-key mhc-calendar-mode-map "n"    'mhc-calendar-next-week)
  (define-key mhc-calendar-mode-map "p"    'mhc-calendar-prev-week)
  (define-key mhc-calendar-mode-map "N"    'mhc-calendar-next-month)
  (define-key mhc-calendar-mode-map "P"    'mhc-calendar-prev-month)
  (define-key mhc-calendar-mode-map ">"    'mhc-calendar-inc-month)
  (define-key mhc-calendar-mode-map "<"    'mhc-calendar-dec-month)
  (define-key mhc-calendar-mode-map "\M-\C-n" 'mhc-calendar-next-year)
  (define-key mhc-calendar-mode-map "\M-\C-p" 'mhc-calendar-prev-year)
  (define-key mhc-calendar-mode-map "\C-@" 'mhc-calendar-set-mark-command)
  (cond
   ((featurep 'xemacs)
    (define-key mhc-calendar-mode-map "\C- " 'mhc-calendar-set-mark-command)
    (define-key mhc-calendar-mode-map [(button1)] 'mhc-calendar-day-at-mouse)
    (define-key mhc-calendar-mode-map [(button2)] 'mhc-calendar-day-at-mouse))
   (t
    (define-key mhc-calendar-mode-map [?\C- ] 'mhc-calendar-set-mark-command)
    (define-key mhc-calendar-mode-map [mouse-1] 'mhc-calendar-day-at-mouse)
    (define-key mhc-calendar-mode-map [mouse-2] 'mhc-calendar-day-at-mouse)))
  (define-key mhc-calendar-mode-map "\C-x\C-x" 'mhc-calendar-exchange-point-and-mark)
  (define-key mhc-calendar-mode-map "q"    'mhc-calendar-quit)
  (define-key mhc-calendar-mode-map "Q"    'mhc-calendar-exit)
  (define-key mhc-calendar-mode-map "?"    'describe-mode))

(unless mhc-calendar-mode-menu-spec
  (setq mhc-calendar-mode-menu-spec
        '("Mhc-Calendar"
          ["Toggle view area" mhc-calendar-goto-home t]
          ["Goto today" mhc-calendar-goto-today t]
          ["Goto next month" mhc-calendar-inc-month t]
          ["Goto prev month" mhc-calendar-dec-month t]
          ["Goto month" mhc-calendar-goto-month t]
          ("Goto"
           ["Next day" mhc-calendar-next-day t]
           ["Prev day" mhc-calendar-prev-day t]
           ["Next week" mhc-calendar-next-week t]
           ["Prev week" mhc-calendar-prev-week t]
           ["Next month" mhc-calendar-next-month t]
           ["Prev month" mhc-calendar-prev-month t]
           ["Next year" mhc-calendar-next-year t]
           ["Prev year" mhc-calendar-prev-year t])
          ["Rescan" mhc-calendar-rescan t]
          ["MHC summary scan" mhc-calendar-scan t]
          "----"
          ["Save to kill ring" mhc-calendar-get-day t]
          ["Insert" mhc-calendar-get-day-insert t]
          ["Insert/Quit" mhc-calendar-get-day-insert-quit t]
          ["Mark set" mhc-calendar-set-mark-command t]
          ["Exchange point & mark" mhc-calendar-exchange-point-and-mark
           mhc-calendar/mark-date t]
          ["Count days in region" mhc-calendar-count-days-region
           mhc-calendar/mark-date t]
          "----"
          ["Goto view area" mhc-calendar-goto-view
           (not (or (mhc-calendar/in-summary-p) (mhc-calendar/in-summary-hnf-p)))]
          ["Schedule view" mhc-calendar-goto-view
           (or (mhc-calendar/in-summary-p) (mhc-calendar/in-summary-hnf-p))]
          ("Schedule edit"
           ["Schedule addition" mhc-calendar-edit
            (or (mhc-calendar/in-date-p) (mhc-calendar/in-summary-p))]
           ["Schedule modify" mhc-calendar-modify (mhc-calendar/in-summary-p)]
           ["Schedule delete" mhc-calendar-delete (mhc-calendar/in-summary-p)]
           ["HNF file edit" mhc-calendar-hnf-edit
            (and mhc-calendar-link-hnf
                 (or (mhc-calendar/in-date-p) (mhc-calendar/in-summary-p)
                     (mhc-calendar/in-summary-hnf-p)))])
          "----"
          ("Misc"
           ["Reset" mhc-reset t]
           ["Quit" mhc-calendar-quit t]
           ["Kill" mhc-calendar-exit t]
           ["Help" describe-mode t]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make rectangle like calendar.el

(defun mhc-calendar-toggle-insert-rectangle (&optional hide-private)
  "Toggle 3 month calendar."
  (interactive "P")
  (setq mhc-insert-calendar (not mhc-insert-calendar))
  (mhc-rescan-month hide-private))

(defun mhc-calendar-setup ()
  (setq mhc-calendar/week-header nil)
  (setq mhc-calendar/separator-str (char-to-string mhc-calendar-separator))
  (mhc-face-put mhc-calendar/separator-str 'mhc-summary-face-separator)
  (if (mhc-calendar/cw-week)
      (when (< mhc-calendar-next-offset 27)
        (setq mhc-calendar-next-offset 27))
    (when (< mhc-calendar-next-offset 23)
      (setq mhc-calendar-next-offset 23)))
  (setq mhc-calendar-width (if (mhc-calendar/cw-week) 28 24))
  (when (mhc-calendar/cw-week)
    (setq mhc-calendar/week-header
          (mhc-calendar/cw-string
           (format "%s " mhc-calendar-cw-indicator))))
  (let ((days (copy-sequence (nthcdr mhc-start-day-of-week
                                     (append mhc-calendar-day-strings
                                             mhc-calendar-day-strings nil))))
        (i 0) day)
    (while (< i 7)
      (setq day (car days))
      (cond
       ((= (% (+ i mhc-start-day-of-week) 7) 0)
        (mhc-face-put day 'mhc-calendar-face-sunday))
       ((= (% (+ i mhc-start-day-of-week) 7) 6)
        (mhc-face-put day 'mhc-calendar-face-saturday))
       (t (mhc-face-put day 'mhc-calendar-face-default)))
      (setq mhc-calendar/week-header
            (concat mhc-calendar/week-header
                    (if mhc-calendar/week-header " ") day))
      (setq days (cdr days))
      (setq i (1+ i)))))

(defun mhc-calendar-insert-rectangle-at (date col &optional months)
  (let ((m nil) (rect nil) (center nil))
    (save-excursion
      (setq date (mhc-date-mm-first date))
      (put-text-property (point-min) (point-max) 'rear-nonsticky t)
      (goto-char (point-min))
      (when mhc-use-wide-scope
        (mhc-summary-search-date date))
      (beginning-of-line)
      (mhc-misc-move-to-column col)
      (if (consp months)
          (setq m (car months)
                center (- m (cdr months)))
        (setq m (or months 3))
        (setq center (/ (1+ m) 2)))
      (while (> m 0)
        (setq rect
              (nconc
               rect
               (mhc-calendar/make-rectangle
                (mhc-date-mm- date (- m center)) mhc-calendar/separator-str)
               (if (> m 1) (list (concat mhc-calendar/separator-str " ")))))
        (setq m (1- m)))
      (mhc-misc-insert-rectangle rect))))

(defun mhc-calendar-make-header (date)
  (let ((ret (mhc-date-format date "%s %04d"
                              (mhc-date-digit-to-mm-string mm t) yy))
        cw)
    (when (eq mhc-calendar-use-cw 'month)
      (setq cw (mhc-calendar/cw-string
                (format "w%d" (mhc-date-cw (mhc-date-mm-first date)))))
      ;; (length   "September 2002 w35") => 18
      ;; (length "Mo Tu We Th Fr Sa Su") => 20
      (setq cw (concat (make-string (- 18 (length ret) (length cw)) ? )
                       cw)))
    (if (mhc-date-yymm= (mhc-date-now) date)
        (mhc-face-put
         ret (mhc-face-get-today-face 'mhc-calendar-face-saturday))
      (mhc-face-put ret 'mhc-calendar-face-saturday))
    (concat "  " (if (mhc-calendar/cw-week) "   " "")
            ret cw)))

(defun mhc-calendar-make-header-ja (date)
  (let ((ret (mhc-date-format date "%04d年%2d月" yy mm))
        (cw ""))
    (when (eq mhc-calendar-use-cw 'month)
      (setq cw (mhc-calendar/cw-string
                (format "  (%d)" (mhc-date-cw (mhc-date-mm-first date))))))
    (if (mhc-date-yymm= (mhc-date-now) date)
        (mhc-face-put
         ret (mhc-face-get-today-face 'mhc-calendar-face-saturday))
      (mhc-face-put ret 'mhc-calendar-face-saturday))
    (concat "    " (if (mhc-calendar/cw-week) "   " "")
            ret cw)))

(defun mhc-calendar/make-rectangle (&optional date separator)
  (let* ((today (mhc-date-now))
         (month (list (concat separator " "
                              mhc-calendar/week-header)
                      (concat separator " "
                              (funcall mhc-calendar-header-function
                                       (or date today)))))
         (mm (mhc-date-mm (or date today)))
         (days (mhc-db-scan-month (mhc-date-yy (or date today)) mm t))
         (separator (if separator separator mhc-calendar/separator-str))
         (start (mhc-day-day-of-week (car days)))
         (i 0)
         week color cw day cdate map)
    (when (mhc-calendar/cw-week)
      (setq cw (mhc-date-cw (mhc-day-date (car days))))
      (setq week (cons (mhc-calendar/cw-string cw) week)))
    (unless (= (mhc-end-day-of-week) 6)
      (setq start (+ start 6))
      (when (> start 6)
        (setq start (- start 7))))
    (while (< i start)
      (setq week (cons "  " week))
      (setq i (1+ i)))
    (while days
      (setq cdate (mhc-day-date (car days)))
      (when (and (null week) (mhc-calendar/cw-week))
        (if (or (eq mm 1) (eq mm 12))
            (setq cw (mhc-date-cw cdate))
          (setq cw (1+ cw)))
        (setq week (cons (mhc-calendar/cw-string cw) week)))
      (setq color
            (cond
             ((= 0 (mhc-day-day-of-week (car days)))
              'mhc-calendar-face-sunday)
             ((mhc-day-holiday (car days))
              (mhc-face-category-to-face "Holiday"))
             ((= 6 (mhc-day-day-of-week (car days)))
              'mhc-calendar-face-saturday)
             (t 'mhc-calendar-face-default)))
      (when (mhc-date= today cdate)
        (setq color (mhc-face-get-today-face color)))
      (when (mhc-day-busy-p (car days))
        (setq color (mhc-face-get-busy-face color)))
      (setq day (format "%2d" (mhc-day-day-of-month (car days))))
      (when color (mhc-face-put day color))
      (add-text-properties 0 (length day)
                           `(mhc-calendar/date-prop ,cdate
                                                    mouse-face ,(if mhc-calendar-use-mouse-highlight
                                                                    'highlight nil)
                                                    help-echo ,(if mhc-calendar-use-help-echo
                                                                   (mhc-calendar/get-contents cdate) nil))
                           day)
      (setq week (cons day week))
      (when (= (mhc-end-day-of-week) (mhc-day-day-of-week (car days)))
        (setq month (cons (mapconcat
                           (function identity)
                           (cons separator (nreverse week))
                           " ")
                          month)
              week nil))
      (setq days (cdr days)))
    (when week
      (setq month (cons (mapconcat
                         (function identity)
                         (cons separator (nreverse week))
                         " ")
                        month)))
    (nreverse month)))

(defun mhc-calendar-mouse-goto-date-view (event)
  (interactive "e")
  (mhc-calendar-mouse-goto-date event 'view))

(eval-and-compile
  (if (featurep 'xemacs)
      (defun mhc-calendar-mouse-icon-function (event)
        (mhc-xmas-icon-call-function event))
    (defun mhc-calendar-mouse-icon-function (event)
      (mhc-e21-icon-call-function event))))

(defun mhc-calendar-mouse-goto-date (event &optional view)
  (interactive "e")
  (let (cdate dayinfo pos cpos func)
    (save-excursion
      (set-buffer (mhc-calendar/event-buffer event))
      (goto-char (mhc-calendar/event-point event))
      (setq cdate (get-text-property (point) 'mhc-calendar/date-prop)))
    (cond
     (cdate
      (unless (= (mhc-current-date-month)
                 (mhc-date-let cdate (mhc-date-new yy mm 1)))
        (mhc-goto-month cdate mhc-default-hide-private-schedules))
      (setq pos (point))
      (goto-char (point-min))
      (setq cpos (point))
      (catch 'detect
        (while (setq cpos (next-single-property-change cpos 'mhc-dayinfo))
          (when (and (setq dayinfo (get-text-property cpos 'mhc-dayinfo))
                     (= cdate (mhc-day-date dayinfo)))
            (setq pos cpos)
            (throw 'detect t))))
      (goto-char pos)
      (funcall (mhc-get-function 'goto-message) view))
     (t
      (unless (mhc-calendar-mouse-icon-function event)
        (setq func (or (lookup-key (current-local-map) (this-command-keys))
                       (lookup-key (current-global-map) (this-command-keys))))
        (when func
          (call-interactively func event)))))))

;; function
(defun mhc-calendar-mode ()
  "\\<mhc-calendar-mode-map>
MHC Calendar mode:: major mode to view calendar and select day.

The keys that are defined for mhc-calendar-mode are:
\\[mhc-calendar-goto-home]      Recover positioning and toggle show 'view area'.
\\[mhc-calendar-goto-today]     Jump to today.
\\[mhc-calendar-inc-month]      Slide to the next month.
\\[mhc-calendar-dec-month]      Slide to the previous month.
\\[mhc-calendar-goto-month]     Jump to your prefer month.
\\[mhc-calendar-rescan] Rescan current calendar.
\\[mhc-calendar-scan]   Scan the point day's schedule summary with MUA.
  If '\\[mhc-calendar-scan]' executed with 'prefix argument', hide private category.

\\[mhc-calendar-next-day]       Goto the next day.
\\[mhc-calendar-prev-day]       Goto the previous day.
\\[mhc-calendar-next-week]      Goto the next week or goto the next summary.
\\[mhc-calendar-prev-week]      Goto previous week or goto the previous summary.
\\[mhc-calendar-next-month]     Goto next month.
\\[mhc-calendar-prev-month]     Goto previous month.
\\[mhc-calendar-next-year]      Goto next year.
\\[mhc-calendar-prev-year]      Goto previous year.
  '\\[mhc-calendar-next-day]' '\\[mhc-calendar-prev-day]' '\\[mhc-calendar-next-week]' '\\[mhc-calendar-prev-week]' '\\[mhc-calendar-next-month]' '\\[mhc-calendar-prev-month]' '\\[mhc-calendar-inc-month]' '\\[mhc-calendar-dec-month]' '\\[mhc-calendar-next-year]' '\\[mhc-calendar-prev-year]'
  effected by 'prefix argument(integer number)'.
\\[mhc-calendar-day-at-mouse]   Day positioning or view schedule file.

\\[mhc-calendar-set-mark-command]       Duration start point set.
\\[mhc-calendar-exchange-point-and-mark]        Duration start point exchange.
\\[mhc-calendar-count-days-region]      Count days in region.

\\[mhc-calendar-get-day]        Get day at point to save kill ring.
\\[mhc-calendar-get-day-insert] Get day at point to insert call buffer.
\\[mhc-calendar-get-day-insert-quit]    Get day at point to insert call buffer, quit.
  if '\\[mhc-calendar-get-day]' '\\[mhc-calendar-get-day-insert]' '\\[mhc-calendar-get-day-insert-quit]' executed with 'prefix argument', means to treat the duration.

\\[mhc-calendar-goto-view]      Goto summary view position or view schedule file.
\\[mhc-calendar-edit]   Create new schdule file.
  If optional argument IMPORT-BUFFER is specified, import its content.
\\[mhc-calendar-modify] Edit the schdule on the cursor point.
\\[mhc-calendar-delete] Delete the schdule on the cursor point.
\\[mhc-calendar-hnf-edit] Edit the Hyper Nikki File.

\\[mhc-reset]   Reset MHC.
\\[mhc-calendar-quit]   Quit and calendar buffer bury.
\\[mhc-calendar-exit]   Quit and calendar buffer kill.
\\[describe-mode]       Show mode help.
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map mhc-calendar-mode-map)
  (make-local-variable 'mhc-calendar-date)
  (make-local-variable 'mhc-calendar-view-date)
  (make-local-variable 'mhc-calendar/mark-date)
  (make-local-variable 'indent-tabs-mode)
  (setq major-mode 'mhc-calendar-mode)
  (setq mode-name "mhc-calendar")
  (setq indent-tabs-mode nil)
  (setq truncate-lines t)
  (when (featurep 'xemacs)
    (easy-menu-add mhc-calendar-mode-menu))
  (unless (memq 'mhc-calendar/duration-show post-command-hook)
    (add-hook 'post-command-hook 'mhc-calendar/duration-show))
  (run-hooks 'mhc-calendar-mode-hook))

(defun mhc-calendar (&optional date)
  "Display 3-month mini calendar."
  (interactive)
  (setq date (or date (mhc-current-date) (mhc-calendar-get-date)))
  (when (and (get-buffer mhc-calendar/buffer) (set-buffer mhc-calendar/buffer))
    (setq date (or date mhc-calendar-view-date))
    (unless (mhc-date-yymm= date mhc-calendar-date)
      (mhc-calendar/create-buffer date)))
  (mhc-calendar/goto-date (or date (mhc-date-now))))

(defun mhc-calendar-goto-today ()
  (interactive)
  (mhc-calendar (mhc-date-now)))

(defun mhc-calendar/goto-date (date)
  (let ((mhc-calendar-view-summary nil) pos)
    (unless (memq 'mhc-calendar/duration-show post-command-hook)
      (add-hook 'post-command-hook 'mhc-calendar/duration-show))
    (unless (get-buffer mhc-calendar/buffer)
      (mhc-calendar/create-buffer date))
    (set-buffer (get-buffer mhc-calendar/buffer))
    (pop-to-buffer mhc-calendar/buffer)
    (while (not pos)
      (setq pos (mhc-calendar/tp-any (point-min) (point-max)
                                     'mhc-calendar/date-prop date))
      (or pos (mhc-calendar/create-buffer date)))
    (goto-char (1+ pos)))
  (setq mhc-calendar-view-date date)
  (save-excursion
    (mhc-calendar/view-summary-delete)
    (when mhc-calendar-view-summary
      (mhc-calendar/view-summary-insert)
      (and mhc-calendar-link-hnf
           (mhc-calendar/hnf-summary-insert))
      (mhc-calendar/put-property-summary)))
  (mhc-calendar/shrink-window))

(defun mhc-calendar/view-summary-delete ()
  (goto-char (point-min))
  (when (re-search-forward "^--" nil t)
    (let ((buffer-read-only nil))
      (beginning-of-line)
      (forward-char -1)
      (set-text-properties (point) (point-max) nil)
      (delete-region (point) (point-max))
      (set-buffer-modified-p nil))))

(defun mhc-calendar/view-summary-insert ()
  (let ((date mhc-calendar-view-date)
        (buffer-read-only nil)
        (mhc-use-week-separator nil))
    (goto-char (point-max))
    (insert "\n")
    (mhc-summary/insert-separator nil nil
                                  (min (1- (window-width))
                                       (* mhc-calendar-next-offset 3)))
    (mhc-summary-make-contents date date 'mhc-calendar)
    (delete-char -1)
    (set-buffer-modified-p nil)))

(defun mhc-calendar/put-property-summary ()
  (condition-case nil
      (when mhc-calendar-use-mouse-highlight
        (let ((buffer-read-only nil)
              beg)
          (goto-char (point-min))
          (when (re-search-forward "^--" nil t)
            (forward-line)
            (while (not (eobp))
              (setq beg (point))
              (end-of-line)
              (put-text-property beg (point) 'mouse-face 'highlight)
              (forward-line))))
        (set-buffer-modified-p nil))
    (error nil)))

(defun mhc-calendar/shrink-window ()
  (or (one-window-p t)
      (/= (frame-width) (window-width))
      (let ((winh (+ (count-lines (point-min) (point-max))
                     mhc-calendar-height-offset)))
        (cond
         ((< winh mhc-calendar-height)
          (setq winh mhc-calendar-height))
         ((< winh window-min-height)
          (setq winh window-min-height)))
        (shrink-window (- (window-height) winh)))))

(defun mhc-calendar/create-buffer (date)
  (set-buffer (get-buffer-create mhc-calendar/buffer))
  (setq buffer-read-only t)
  (unless (eq major-mode 'mhc-calendar-mode)
    (mhc-calendar-mode)
    (buffer-disable-undo))
  (or (mhc-date-p date) (setq date (mhc-date-now)))
  (let ((buffer-read-only nil)
        (caldate (mhc-date-mm+ date -1))
        (col mhc-calendar-start-column)
        (prefix " +|")
        (i 3))
    (mhc-calendar/delete-overlay)
    (set-text-properties (point-min) (point-max) nil)
    (erase-buffer)
    (message "mhc-calendar create...")
    (while (> i 0)
      (goto-char (point-min))
      (mhc-misc-move-to-column col)
      (mhc-misc-insert-rectangle
       (mhc-calendar/make-rectangle caldate (if (= i 3) "" "|")))
      (setq caldate (mhc-date-mm+ caldate 1))
      (setq col (- (+ col mhc-calendar-next-offset) (if (= i 3) 1 0)))
      (setq i (1- i)))
    (goto-char (point-min))
    (while (re-search-forward prefix nil t)
      (delete-region (match-end 0) (match-beginning 0))
      (insert (make-string (- (match-end 0) (match-beginning 0)) ?\ )))
    (setq mhc-calendar-date date)
    ;; (mhc-calendar/put-property-date)
    (and mhc-calendar-link-hnf (mhc-calendar/hnf-mark-diary-entries))
    (run-hooks 'mhc-calendar-create-buffer-hook)
    (set-buffer-modified-p nil)
    (message "mhc-calendar create...done")))

(defvar mhc-calendar/date-format nil)

(defun mhc-calendar/get-contents (date)
  (unless mhc-calendar/date-format
    (setq mhc-calendar/date-format
          (if (eq mhc-calendar-language 'japanese)
              "%04d年%2d月%2d日(%s)\n"
            "%04d-%02d-%02d (%s)\n")))
  (with-temp-buffer
    (let* ((dayinfo (car (mhc-db-scan date date)))
           (schedules (mhc-day-schedules dayinfo))
           schedule begin end subject location)
      (mhc-date-let (mhc-day-date dayinfo)
        (insert (format mhc-calendar/date-format
                        yy mm dd
                        (aref mhc-calendar-day-strings ww))))
      (when schedules (insert "\n"))
      (while (setq schedule (car schedules))
        (setq schedules (cdr schedules))
        (setq begin (mhc-schedule-time-begin schedule))
        (setq end (mhc-schedule-time-end schedule))
        (setq subject (or (mhc-schedule-subject schedule) ""))
        (setq location (or (mhc-schedule-location schedule) ""))
        (when (> (length location) 0)
          (setq location (concat " [" location "]")))
        (when (or begin end subject location)
          (insert (format "%s%s%s%s%s\n"
                          (if begin
                              (format "%02d:%02d" (/ begin 60) (% begin 60))
                            "")
                          (if end
                              (format "-%02d:%02d" (/ end 60) (% end 60))
                            "")
                          (if (or begin end) " " "")
                          subject location))))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun mhc-calendar-edit ()
  (interactive)
  (if (or (mhc-calendar/in-date-p)
          (mhc-calendar/in-summary-p))
      (progn
        (mhc-window-push)
        (mhc-edit nil)
        (delete-other-windows))
    (message "Nothing to do in this point.")))

(defun mhc-calendar-delete ()
  (interactive)
  (let ((filename (mhc-calendar/in-summary-p)) key)
    (if (null filename)
        (message "Nothing to do in this point.")
      (setq key (mhc-slot-directory-to-key
                 (directory-file-name (file-name-directory filename))))
      (mhc-delete-file
       (assoc filename (mhc-slot-records (mhc-slot-get-month-schedule key)))))))

(defun mhc-calendar-modify ()
  (interactive)
  (if (mhc-calendar/in-summary-p)
      (mhc-modify-file (mhc-calendar/in-summary-p))
    (message "Nothing to do in this point.")))

(defun mhc-calendar-toggle-view ()
  (interactive)
  (setq mhc-calendar-view-summary (not mhc-calendar-view-summary))
  (mhc-calendar/goto-date (mhc-calendar-get-date)))

(defun mhc-calendar-goto-view ()
  (interactive)
  (cond
   ((mhc-calendar/in-summary-p)
    (mhc-calendar/view-file (mhc-calendar/in-summary-p)))
   ((mhc-calendar/in-summary-hnf-p)
    (mhc-calendar/hnf-view))
   (t
    (setq mhc-calendar-view-summary t)
    (mhc-calendar/goto-date (mhc-calendar-get-date))
    (goto-char (next-single-property-change
                (point) 'mhc-calendar/summary-prop)))))

(defun mhc-calendar/view-file (file)
  (if (and (stringp file) (file-exists-p file))
      (let ((newname (mhc-date-format
                      mhc-calendar-view-date "+%04d/%02d/%02d" yy mm dd)))
        (mhc-window-push)
        (view-file-other-window file)
        ;; eword decode
        (mhc-calendar/view-file-decode-header)
        (setq view-exit-action 'mhc-calendar-view-exit-action)
        (set-visited-file-name nil)
        (rename-buffer newname 'unique)
        (run-hooks 'mhc-calendar-view-file-hook)
        (set-buffer-modified-p nil)
        (setq buffer-read-only t))
    (message "File does not exist (%s)." file)))

(defun mhc-calendar/view-file-decode-header ()
  (let ((buffer-read-only nil))
    (goto-char (point-min))
    (mhc-decode-header)
    (mhc-highlight-message)))

;; insert function
(defun mhc-calendar-get-day (&optional arg)
  (interactive "P")
  (let (str)
    (if (null arg)
        (setq str (mhc-calendar/get-day))
      (setq str (mhc-calendar/get-day-region)))
    (kill-new str)
    (message "\"%s\" to the latest kill in the kill ring." str)))

(defun mhc-calendar-get-day-insert-quit (&optional arg)
  (interactive "P")
  (when (mhc-calendar-get-day-insert arg)
    (mhc-calendar-quit)))

(defun mhc-calendar-get-day-insert (&optional arg)
  (interactive "P")
  (let ((callbuf mhc-calendar/inserter-call-buffer)
        (type mhc-calendar/inserter-type)
        (defbuff (buffer-name
                  (car (delete (get-buffer mhc-calendar/buffer)
                               (buffer-list)))))
        str)
    ;; in mhc-calendar/buffer
    (if (null arg)
        (setq str (mhc-calendar/get-day type))
      (setq str (mhc-calendar/get-day-region type)))
    (kill-new str)
    (unless (and callbuf (get-buffer callbuf) (buffer-name callbuf))
      (setq callbuf (read-buffer "Insert buffer? " defbuff t)))
    ;; in mhc-clendar-call-buffer
    (if (not (get-buffer callbuf))
        (message "No buffer detect \"%s\"" callbuf)
      (set-buffer (get-buffer callbuf))
      (pop-to-buffer callbuf)
      (cond
       ((window-minibuffer-p)
        (insert str) t)
       (t (condition-case err
              (progn
                (insert str)
                (message "\"%s\" insert done." str) t)
            (error
             (pop-to-buffer (get-buffer mhc-calendar/buffer))
             (message "\"%s\" insert failed." str) nil)))))))

(defun mhc-calendar/get-day (&optional type)
  (let ((date (mhc-calendar-get-date))
        datelst rlst)
    (cond
     ((eq type 'minibuffer)
      (setq datelst mhc-calendar/inserter-for-minibuffer))
     ((or (eq type 'duration) (eq type 'day))
      (setq datelst mhc-calendar/inserter-for-draft))
     (t (setq datelst mhc-calendar-inserter-date-list)))
    (setq rlst (mhc-calendar/get-day-list date datelst))
    (mhc-calendar/get-day-select rlst)))

(defun mhc-calendar/get-day-region (&optional type)
  (let (cat datebeg dateend datetmp datelst rlst)
    (if (not (mhc-date-p mhc-calendar/mark-date))
        (error "No mark set in this buffer")
      (setq dateend (mhc-calendar-get-date))
      (setq datebeg mhc-calendar/mark-date)
      ;; swap
      (when (mhc-date> datebeg dateend)
        (setq datetmp dateend)
        (setq dateend datebeg)
        (setq datebeg datetmp))
      (if (eq type 'day)
          ;; for X-SC-Day: (20000101 200000102 ... 20000131)
          (progn
            (setq datetmp nil)
            (while (mhc-date<= datebeg dateend)
              (setq datetmp (cons datebeg datetmp))
              (setq datebeg (mhc-date++ datebeg)))
            (mapconcat
             (lambda (x)
               (mhc-date-format x "%04d%02d%02d" yy mm dd))
             (nreverse datetmp) " "))
        (cond
         ((eq type 'minibuffer)
          (setq datelst mhc-calendar/inserter-for-minibuffer))
         ((eq type 'duration)
          (setq datelst mhc-calendar/inserter-for-draft))
         (t (setq datelst mhc-calendar-inserter-date-list)))
        (setq rlst (mhc-calendar/get-day-list datebeg datelst dateend))
        (mhc-calendar/get-day-select rlst)))))

(defun mhc-calendar/get-day-select (lst)
  (cond
   ((= (length lst) 0) (error "Something error occur."))
   ((= (length lst) 1) (car lst))
   (t
    (let ((i 0)
          (completion-ignore-case nil)
          alst hist cell input)
      (while lst
        (setq cell (format "%d: %s" i (car lst)))
        (setq hist (cons cell hist))
        (setq alst (cons (cons cell (car lst)) alst))
        (setq i (1+ i))
        (setq lst (cdr lst)))
      (setq hist (nreverse hist))
      (setq alst (nreverse alst))
      (setq mhc-calendar/select-alist alst) ;; for completion
      (setq input (mhc-calendar/select-comp "Select format: " 'active))
      (when (string= input "")
        (setq input (cdr (car alst))))
      (when (string-match "^\\([0-9]+\\)$" input)
        (setq i (string-to-int input))
        (when (> (length alst) i)
          (setq input (cdr (nth i alst)))))
      (when (string-match "^[0-9]+:[ \t]*" input)
        (setq input (substring input (match-end 0))))
      input))))

(defun mhc-calendar-count-days-region ()
  (interactive)
  (let ((mark mhc-calendar/mark-date)
        (date (mhc-calendar-get-date)))
    (if (null mark)
        (error "No mark set in this buffer")
      (setq date (mhc-date++ (mhc-date- (max mark date) (min mark date))))
      (kill-new (int-to-string date))
      (if (< date 7)
          (message "%d days in region." date)
        (if (= (% date 7) 0)
            (message "%d days (%d weeks) in region." date (/ date 7))
          (message "%d days (%d weeks + %d days) in region."
                   date (/ date 7) (% date 7)))))))

;; selector
(defvar mhc-calendar/select-alist nil)
(defvar mhc-calendar/select-hist nil)
(defvar mhc-calendar/select-map nil)
(defvar mhc-calendar/select-buffer "*Completions*")

(if mhc-calendar/select-map
    ()
  (setq mhc-calendar/select-map (make-sparse-keymap))
  (define-key mhc-calendar/select-map "\t"   'mhc-calendar/select-comp-window)
  (define-key mhc-calendar/select-map "\r"   'exit-minibuffer)
  (define-key mhc-calendar/select-map "\n"   'exit-minibuffer)
  (define-key mhc-calendar/select-map "\C-g" 'abort-recursive-edit)
  (define-key mhc-calendar/select-map "\M-s" 'next-matching-history-element)
  (define-key mhc-calendar/select-map "\M-p" 'previous-history-element)
  (define-key mhc-calendar/select-map "\M-n" 'next-history-element)
  (define-key mhc-calendar/select-map "\M-v" 'switch-to-completions))

(defun mhc-calendar/select-comp-setup ()
  (mhc-calendar/select-comp-window ""))

(defun mhc-calendar/select-comp-window (&optional word)
  (interactive)
  (let ((completion-ignore-case nil)
        outp pos)
    (when (not word)
      (setq word (buffer-substring-no-properties
                  (save-excursion (beginning-of-line) (point))
                  (point-max)))
      (setq outp (try-completion word mhc-calendar/select-alist))
      (when (and (stringp outp)
                 (window-minibuffer-p (get-buffer-window (current-buffer))))
        (beginning-of-line)
        (delete-region (point) (point-max))
        (insert outp)))
    (with-output-to-temp-buffer mhc-calendar/select-buffer
      (display-completion-list
       (all-completions word mhc-calendar/select-alist)))))

(defvar mhc-calendar/select-comp-active nil)
(defadvice choose-completion-string (around mhc-calendar-select activate)
  ad-do-it
  (when mhc-calendar/select-comp-active
    (select-window (active-minibuffer-window))))

(defun mhc-calendar/select-comp (&optional prompt active)
  (let ((minibuffer-setup-hook minibuffer-setup-hook)
        (ret ""))
    (unless prompt (setq prompt "Select: "))
    (unwind-protect
        (progn
          ;; Select minibuffer forcibly
          (setq mhc-calendar/select-comp-active t)
          ;; completion buffer setup
          (when active
            (add-hook 'minibuffer-setup-hook 'mhc-calendar/select-comp-setup))
          (setq ret (read-from-minibuffer
                     prompt
                     nil mhc-calendar/select-map nil 'mhc-calendar/select-hist)))
      (setq mhc-calendar/select-comp-active nil)
      (remove-hook 'minibuffer-setup-hook
                   'mhc-calendar/select-comp-setup)
      (and (buffer-live-p (get-buffer mhc-calendar/select-buffer))
           (kill-buffer mhc-calendar/select-buffer))
      ret)))

;; inserter
(defun mhc-calendar/get-day-list-func (form)
  (let (func)
    (cond
     ((stringp form) form)
     ((symbolp form)
      (setq func (intern-soft
                  (concat "mhc-calendar/inserter-" (symbol-name form))))
      (and func (funcall func))))))

(defun mhc-calendar/inserter-yy ()
  (format "%4d" yy))

(defun mhc-calendar/inserter-nengo ()
  (if (> yy 1987)
      (format "平成%2d年" (- yy 1988))
    (if (> yy 1924)
        (format "昭和%2d年" (- yy 1925))
      "昔々")))

(defun mhc-calendar/inserter-mm ()
  (format "%d" mm))

(defun mhc-calendar/inserter-mm02 ()
  (format "%02d" mm))

(defun mhc-calendar/inserter-mm2 ()
  (format "%2d" mm))

(defun mhc-calendar/inserter-mm-string ()
  (mhc-date-digit-to-mm-string mm))

(defun mhc-calendar/inserter-mm-string-long ()
  (mhc-date-digit-to-mm-string mm t))

(defun mhc-calendar/inserter-dd ()
  (format "%d" dd))

(defun mhc-calendar/inserter-dd02 ()
  (format "%02d" dd))

(defun mhc-calendar/inserter-dd2 ()
  (format "%2d" dd))

(defun mhc-calendar/inserter-ww ()
  (format "%d" ww))

(defun mhc-calendar/inserter-ww-string ()
  (mhc-date-digit-to-ww-string ww))

(defun mhc-calendar/inserter-ww-string-long ()
  (mhc-date-digit-to-ww-string ww t))

(defun mhc-calendar/inserter-ww-japanese ()
  (mhc-date-digit-to-ww-japanese-string ww))

(defun mhc-calendar/inserter-ww-japanese-long ()
  (mhc-date-digit-to-ww-japanese-string ww t))

(defun mhc-calendar/get-day-list (date &optional datelst dateend)
  (let (lst-org formlst retlst retlst2 ret con)
    (setq lst-org (or datelst mhc-calendar-inserter-date-list))
    (setq datelst lst-org)
    ;; begin
    (mhc-date-let date
      (while datelst
        (setq formlst (car (car datelst)))
        (setq ret nil)
        (while formlst
          (setq ret (concat
                     ret (mhc-calendar/get-day-list-func (car formlst))))
          (setq formlst (cdr formlst)))
        (setq retlst (cons ret retlst))
        (setq datelst (cdr datelst))))
    (setq retlst (nreverse retlst))
    (if (not dateend)
        retlst ;; return
      ;; duration
      (setq datelst lst-org)
      (mhc-date-let dateend
        (while datelst
          (setq con (cdr (car datelst)))
          (if (listp con) ;; multiple connectoer
              (while con
                (setq formlst (car (car datelst)))
                (setq ret (car con))
                (while formlst
                  (setq ret (concat
                             ret (mhc-calendar/get-day-list-func (car formlst))))
                  (setq formlst (cdr formlst)))
                (setq retlst2 (cons (concat (car retlst) ret) retlst2))
                (setq con (cdr con)))
            (setq formlst (car (car datelst)))
            (setq ret (cdr (car datelst)))
            (while formlst
              (setq ret (concat
                         ret (mhc-calendar/get-day-list-func (car formlst))))
              (setq formlst (cdr formlst)))
            (setq retlst2 (cons (concat (car retlst) ret) retlst2)))
          (setq retlst (cdr retlst))
          (setq datelst (cdr datelst))))
      (nreverse retlst2))))

;; scan & move functions
(defun mhc-calendar-scan (&optional hide-private)
  (interactive "P")
  (let ((date (mhc-calendar-get-date)))
    (mhc-calendar-quit)
    (mhc-goto-month date hide-private)
    (goto-char (point-min))
    (if (mhc-summary-search-date date)
        (progn
          (beginning-of-line)
          (if (not (pos-visible-in-window-p (point)))
              (recenter))))))

(defun mhc-calendar-quit ()
  (interactive)
  (let ((win (get-buffer-window mhc-calendar/buffer))
        (buf (get-buffer mhc-calendar/buffer)))
    (save-excursion
      (set-buffer buf)
      (mhc-calendar/delete-overlay))
    (if (null win)
        ()
      (bury-buffer buf)
      (if (null (one-window-p))
          (delete-windows-on buf)
        (set-window-buffer win (other-buffer))
        (select-window (next-window))))))

(defun mhc-calendar-input-exit ()
  (setq mhc-calendar/inserter-type nil)
  (setq mhc-calendar/inserter-call-buffer nil))

(defun mhc-calendar-exit ()
  (interactive)
  (mhc-calendar-quit)
  (remove-hook 'post-command-hook 'mhc-calendar/duration-show)
  (kill-buffer (get-buffer mhc-calendar/buffer)))

(defun mhc-calendar-goto-month (&optional date)
  (interactive)
  (mhc-calendar/goto-date (if (integerp date) date (mhc-input-month "Month "))))

(defun mhc-calendar-rescan ()
  (interactive)
  (set-buffer (get-buffer mhc-calendar/buffer))
  (let ((cdate mhc-calendar-date)
        (pdate (mhc-calendar-get-date)))
    (setq mhc-calendar-date nil)
    (mhc-calendar/create-buffer cdate)
    (mhc-calendar/goto-date pdate)))

(defun mhc-calendar-goto-home ()
  (interactive)
  (setq mhc-calendar-view-summary
        (not (and (eq last-command 'mhc-calendar-goto-home)
                  mhc-calendar-view-summary)))
  (mhc-calendar/goto-date (mhc-calendar-get-date))
  (set-window-start (selected-window) (point-min)))

(defun mhc-calendar-next-day (&optional arg)
  (interactive "p")
  (let ((date (mhc-calendar-get-date)))
    (mhc-calendar/goto-date (+ date arg))))

(defun mhc-calendar-prev-day (&optional arg)
  (interactive "p")
  (mhc-calendar-next-day (- arg)))

(defun mhc-calendar-next-week (&optional arg)
  (interactive "p")
  (if (or (mhc-calendar/in-summary-p) (mhc-calendar/in-summary-hnf-p))
      (let ((pos (point)))
        (forward-line)
        (if (eobp) (goto-char pos)))
    (mhc-calendar-next-day (* arg 7))))

(defun mhc-calendar-prev-week (&optional arg)
  (interactive "p")
  (if (or (mhc-calendar/in-summary-p) (mhc-calendar/in-summary-hnf-p))
      (let ((pos (point)))
        (forward-line -1)
        (if (or (mhc-calendar/in-summary-p) (mhc-calendar/in-summary-hnf-p))
            ()
          (goto-char pos)))
    (mhc-calendar-next-day (- (* arg 7)))))

(defun mhc-calendar-next-month (&optional arg)
  (interactive "p")
  (mhc-calendar/goto-date (mhc-date-mm+ (mhc-calendar-get-date) arg)))

(defun mhc-calendar-prev-month (&optional arg)
  (interactive "p")
  (mhc-calendar-next-month (- arg)))

(defun mhc-calendar-next-year (&optional arg)
  (interactive "p")
  (mhc-calendar/goto-date (mhc-date-yy+ (mhc-calendar-get-date) arg)))

(defun mhc-calendar-prev-year  (&optional arg)
  (interactive "p")
  (mhc-calendar-next-year (- arg)))

(defun mhc-calendar-inc-month (&optional arg)
  (interactive "p")
  (set-buffer (get-buffer mhc-calendar/buffer))
  (let* ((dnew (mhc-date-mm+ mhc-calendar-date arg))
         (ddold (mhc-date-dd (mhc-calendar-get-date)))
         (dnew2 (mhc-date-let dnew
                  (if (mhc-date/check yy mm ddold)
                      (mhc-date-new yy mm ddold)
                    (mhc-date-new  yy mm (mhc-date/last-day-of-month yy mm))))))
    (mhc-calendar/create-buffer dnew)
    (mhc-calendar/goto-date dnew2)))

(defun mhc-calendar-dec-month (&optional arg)
  (interactive "p")
  (mhc-calendar-inc-month (- arg)))

(defun mhc-calendar-get-date ()
  (when (mhc-calendar-p)
    (if (mhc-calendar/in-date-p)
        (mhc-calendar/in-date-p)
      (if (or (mhc-calendar/in-summary-p) (mhc-calendar/in-summary-hnf-p))
          mhc-calendar-view-date
        (let* ((pos (point))
               (col (current-column))
               (colnum (mhc-calendar/get-date-colnum col))
               (line (+ (count-lines (point-min) (point)) (if (= col 0) 1 0)))
               (date (mhc-date-mm+ mhc-calendar-date colnum))
               (date1 (mhc-date-mm-first date))
               (datelast (mhc-date-mm-last date))
               daypos)
          (cond
           ((< line 3) date1)
           ((> line 9) datelast)
           (t
            (setq daypos (next-single-property-change (point) 'mhc-calendar/date-prop))
            (if daypos
                (progn
                  (goto-char daypos)
                  (if (= colnum (mhc-calendar/get-date-colnum (current-column)))
                      (mhc-calendar/in-date-p)
                    (goto-char pos)
                    (if (or (and (goto-char (previous-single-property-change
                                             (point) 'mhc-calendar/date-prop))
                                 (mhc-calendar/in-date-p))
                            (and (goto-char (previous-single-property-change
                                             (point) 'mhc-calendar/date-prop))
                                 (mhc-calendar/in-date-p)))
                        (if (= colnum (mhc-calendar/get-date-colnum (current-column)))
                            (mhc-calendar/in-date-p)
                          datelast)
                      datelast)))
              datelast))))))))

(defun mhc-calendar-view-date ()
  (and (mhc-calendar-p) mhc-calendar-view-date))

;; mouse function
(defun mhc-calendar-day-at-mouse (event)
  (interactive "e")
  (set-buffer (mhc-calendar/event-buffer event))
  (pop-to-buffer (mhc-calendar/event-buffer event))
  (goto-char (mhc-calendar/event-point event))
  (cond
   ((mhc-calendar/in-date-p)
    (mhc-calendar-goto-home))
   ((mhc-calendar/in-summary-p)
    (mhc-calendar/view-file (mhc-calendar/in-summary-p)))
   ((mhc-calendar/in-summary-hnf-p)
    (mhc-calendar/hnf-view))
   (t (message "Nothing to do in this point."))))

;; mark
(defun mhc-calendar-set-mark-command (arg)
  (interactive "P")
  (if (null arg)
      (progn
        (setq mhc-calendar/mark-date (mhc-calendar-get-date))
        (message "Mark set"))
    (setq mhc-calendar/mark-date nil)
    (mhc-calendar/duration-show)
    (message "Mark unset")))

(defun mhc-calendar-exchange-point-and-mark ()
  (interactive)
  (let ((mark mhc-calendar/mark-date)
        (date (mhc-calendar-get-date)))
    (if (null mark)
        (error "No mark set in this buffer")
      (setq mhc-calendar/mark-date date)
      (mhc-calendar/goto-date mark)
      (mhc-calendar/duration-show))))

;; post-command-hook
(defun mhc-calendar/duration-show ()
  (when (eq this-command 'keyboard-quit)
    (setq mhc-calendar/mark-date nil))
  (if (not (mhc-calendar-p))
      (remove-hook 'post-command-hook 'mhc-calendar/duration-show)
    (when (mhc-calendar-p)
      (mhc-calendar/delete-overlay)
      (setq mode-name "mhc-calendar")
      (when (and mhc-calendar-use-duration-show mhc-calendar/mark-date)
        (let ((datebeg mhc-calendar/mark-date)
              (dateend (point))
              datetmp pos)
          (save-excursion
            (goto-char dateend)
            (setq dateend (mhc-calendar-get-date))
            (when (and datebeg dateend
                       (not (mhc-date= datebeg dateend)))
              (when (mhc-date> datebeg dateend)
                (setq datetmp dateend)
                (setq dateend datebeg)
                (setq datebeg datetmp))
              (when (or (eq mhc-calendar-use-duration-show 'modeline)
                        (eq mhc-calendar-use-duration-show 'mixed))
                (setq mode-name
                      (format "mhc-calendar %s-%s"
                              (mhc-date-format
                               datebeg "%04d/%02d/%02d(%s)"
                               yy mm dd (mhc-date-digit-to-ww-string ww))
                              (mhc-date-format
                               dateend "%04d/%02d/%02d(%s)"
                               yy mm dd (mhc-date-digit-to-ww-string ww)))))
              (when (or (eq mhc-calendar-use-duration-show 'face)
                        (eq mhc-calendar-use-duration-show 'mixed))
                (goto-char (point-min))
                (setq datetmp (mhc-calendar-get-date))
                (if (mhc-date< datebeg datetmp)
                    (setq datebeg datetmp))
                (setq pos t)
                (while (and pos (mhc-date<= datebeg dateend))
                  (setq pos (mhc-calendar/tp-any
                             (point-min) (point-max)
                             'mhc-calendar/date-prop datebeg))
                  (when pos
                    (overlay-put (make-overlay pos (+ pos 2))
                                 'face 'mhc-calendar-face-duration))
                  (setq datebeg (mhc-date++ datebeg)))))))
        (when (or (eq mhc-calendar-use-duration-show 'modeline)
                  (eq mhc-calendar-use-duration-show 'mixed))
          (force-mode-line-update))))))

;; misc
(defun mhc-calendar/delete-overlay ()
  (when (or (eq mhc-calendar-use-duration-show 'face)
            (eq mhc-calendar-use-duration-show 'mixed))
    (let ((ovlin (overlays-in (point-min) (point-max))))
      (while ovlin
        (delete-overlay (car ovlin))
        (setq ovlin (cdr ovlin))))))

(defun mhc-calendar/delete-region (yy mm dd pos)
  (condition-case err
      (if (mhc-date/check yy mm dd)
          (progn
            (delete-region (point) pos)
            (mhc-date-new yy mm dd))
        nil)
    (error nil)))

(defun mhc-calendar-view-exit-action (buff)
  (kill-buffer buff)
  (and (get-buffer mhc-calendar/buffer) (mhc-window-pop)))

;; mhc-minibuffer support
(defun mhc-minibuf-insert-calendar ()
  (interactive)
  (let ((yy 1) (mm 1) (dd 1) date pos)
    (setq mhc-calendar/inserter-type 'minibuffer)
    (setq mhc-calendar/inserter-call-buffer (current-buffer))
    (save-excursion
      (setq pos (point))
      (skip-chars-backward "0-9/")
      (cond
       ((looking-at "\\([12][0-9][0-9][0-9]\\)/\\([0-2][0-9]\\)/\\([0-3][0-9]\\)")
        (setq yy (mhc-calendar/buffer-substring-to-num 1))
        (setq mm (mhc-calendar/buffer-substring-to-num 2))
        (setq dd (mhc-calendar/buffer-substring-to-num 3))
        (setq date (mhc-calendar/delete-region yy mm dd pos)))
       ((looking-at "\\([12][0-9][0-9][0-9]\\)/\\([0-2][0-9]\\)/?")
        (setq yy (mhc-calendar/buffer-substring-to-num 1))
        (setq mm (mhc-calendar/buffer-substring-to-num 2))
        (setq date (mhc-calendar/delete-region yy mm dd pos)))
       ((looking-at "\\([12][0-9][0-9][0-9]\\)/?")
        (setq yy (mhc-calendar/buffer-substring-to-num 1))
        (setq date (mhc-calendar/delete-region yy mm dd pos)))))
    (mhc-calendar date)))

;; mhc-draft support
(defun mhc-draft-insert-calendar ()
  (interactive)
  (let ((yy 1) (mm 1) (dd 1)
        (case-fold-search t)
        date pos)
    (setq mhc-calendar/inserter-call-buffer (current-buffer))
    (setq mhc-calendar/inserter-type nil)
    (save-excursion
      (setq pos (point))
      (goto-char (point-min))
      (if (and (re-search-forward
                (concat "^" (regexp-quote mail-header-separator) "$\\|^$") nil t)
               (< pos (point)))
          (progn
            (setq mhc-calendar/inserter-type 'duration)
            (save-excursion
              (goto-char pos)
              (and (re-search-backward "x-[^:]+: " nil t)
                   (looking-at "^x-sc-day: ")
                   (setq mhc-calendar/inserter-type 'day)))))
      (goto-char pos)
      (skip-chars-backward "0-9")
      (cond
       ((looking-at "\\([12][0-9][0-9][0-9]\\)\\([0-2][0-9]\\)\\([0-3][0-9]\\)")
        (setq yy (mhc-calendar/buffer-substring-to-num 1))
        (setq mm (mhc-calendar/buffer-substring-to-num 2))
        (setq dd (mhc-calendar/buffer-substring-to-num 3))
        (setq date (mhc-calendar/delete-region yy mm dd pos)))
       ((looking-at "\\([12][0-9][0-9][0-9]\\)\\([0-2][0-9]\\)")
        (setq yy (mhc-calendar/buffer-substring-to-num 1))
        (setq mm (mhc-calendar/buffer-substring-to-num 2))
        (setq date (mhc-calendar/delete-region yy mm dd pos)))
       ((looking-at "\\([12][0-9][0-9][0-9]\\)")
        (setq yy (mhc-calendar/buffer-substring-to-num 1))
        (setq date (mhc-calendar/delete-region yy mm dd pos)))))
    (mhc-calendar date)))

;; hnf-mode interface
(defun mhc-calendar/hnf-get-filename (date)
  (expand-file-name
   (mhc-date-format date "d%04d%02d%02d.hnf" yy mm dd)
   (if hnf-diary-year-directory-flag
       (expand-file-name (mhc-date-format date "%04d" yy) hnf-diary-dir)
     hnf-diary-dir)))

(defun mhc-calendar/hnf-file-list (date)
  (let ((i -1) flst)
    (setq date (mhc-date-mm+ date -1))
    (while (< i 2)
      (let* ((dir (if hnf-diary-year-directory-flag
                      (expand-file-name (mhc-date-format date "%04d" yy) hnf-diary-dir)
                    (expand-file-name hnf-diary-dir)))
             (fnexp (mhc-date-format date "d%04d%02d[0-3][0-9]\\.hnf" yy mm)))
        (if (file-directory-p dir)
            (setq flst (append (directory-files dir nil fnexp 'no-sort) flst))
          (setq flst nil))
        (setq date (mhc-date-mm+ date 1))
        (setq i (1+ i))))
    flst))

(defvar mhc-calendar/hnf-ignore-categories nil)

(defun mhc-calendar-hnf-edit (&optional args)
  (interactive "P")
  (if (not mhc-calendar-link-hnf)
      (message "Nothing to do.")
    (let ((hnffile (mhc-calendar/hnf-get-filename (mhc-calendar-get-date)))
          (mhcfile (mhc-calendar/in-summary-p))
          (count (mhc-calendar/in-summary-hnf-p))
          cats subj uri lst)
      (save-excursion
        (when (and args mhcfile (file-readable-p mhcfile))
          (unless mhc-calendar/hnf-ignore-categories
            (setq lst mhc-icon-function-alist)
            (while lst
              (setq mhc-calendar/hnf-ignore-categories
                    (cons (downcase (car (car lst)))
                          mhc-calendar/hnf-ignore-categories))
              (setq lst (cdr lst))))
          (with-temp-buffer
            (insert-file-contents mhcfile)
            (mhc-decode-header)
            (mhc-header-narrowing
              (setq cats (mhc-header-get-value "x-sc-category"))
              (setq subj (mhc-header-get-value "x-sc-subject"))
              (setq lst (mhc-misc-split cats))
              (when (member "Link" lst)
                (setq uri (or (mhc-header-get-value "x-uri")
                              (mhc-header-get-value "x-url"))))
              (setq cats nil)
              (while lst
                (unless (member (downcase (car lst))
                                mhc-calendar/hnf-ignore-categories)
                  (setq cats (cons (car lst) cats)))
                (setq lst (cdr lst)))
              (setq cats (nreverse cats))))))
      (find-file-other-window hnffile)
      (hnf-mode)
      (and (integerp count) (mhc-calendar/hnf-search-title count))
      (when subj
        (goto-char (point-max))
        (insert "\n")
        (when cats
          (insert (format "CAT %s\n"
                          (mapconcat 'identity cats " "))))
        (if uri
            (insert (format "LNEW %s %s\n" uri subj))
          (insert (format "NEW %s\n" subj)))))))
;; xxxxx

(defun mhc-calendar/hnf-view ()
  (interactive)
  (let ((fname (mhc-calendar/hnf-get-filename (mhc-calendar-get-date)))
        (count (mhc-calendar/in-summary-hnf-p)))
    (if (not (file-readable-p fname))
        (message "File does not exist (%s)." fname)
      (mhc-window-push)
      (view-file-other-window fname)
      (setq view-exit-action 'mhc-calendar-view-exit-action)
      (and (integerp count) (mhc-calendar/hnf-search-title count)))))

(defun mhc-calendar/hnf-search-title (count)
  (goto-char (point-min))
  (while (and (> count 0) (not (eobp)))
    (re-search-forward "^\\(L?NEW\\|L?SUB\\)[ \t]+" nil t)
    (setq count (1- count)))
  (beginning-of-line)
  (recenter (/ (window-height) 4)))

(defun mhc-calendar/hnf-mark-diary-entries ()
  (let ((cdate (mhc-date-mm-first (mhc-date-mm+ mhc-calendar-date -1)))
        (edate (mhc-date-mm-last (mhc-date-mm+ mhc-calendar-date 1)))
        (flst (mhc-calendar/hnf-file-list mhc-calendar-date))
        (mark "'"))
    (mhc-face-put mark 'mhc-calendar-hnf-face-mark)
    (while (<= cdate edate)
      (if (member (mhc-date-format cdate "d%04d%02d%02d.hnf" yy mm dd) flst)
          (progn
            (goto-char (+ 2 (mhc-calendar/tp-any (point-min) (point-max)
                                                 'mhc-calendar/date-prop cdate)))
            (insert mark)
            (if (eq (char-after (point)) ?\ )
                (delete-char 1))))
      (setq cdate (1+ cdate)))))

(defun mhc-calendar/hnf-summary-insert ()
  (let ((fname (mhc-calendar/hnf-get-filename mhc-calendar-view-date))
        (buffer-read-only nil)
        (newmark "#") (sub "＠") (cat "")
        (count 1) (ncount 1)
        new summary str uri)
    (if (not (file-readable-p fname))
        ()
      (goto-char (point-max))
      (with-temp-buffer ;; hnf-mode.el require APEL :-)
        (insert-file-contents fname)
        (goto-char (point-min))
        (mhc-face-put sub 'mhc-calendar-hnf-face-subtag)
        (while (not (eobp))
          (cond
           ;; CAT
           ((looking-at "^CAT[ \t]+\\(.*\\)$")
            (setq cat (buffer-substring (match-beginning 1) (match-end 1)))
            (while (string-match "[ \t]+" cat)
              (setq cat (concat (substring cat 0 (match-beginning 0))
                                "]["
                                (substring cat (match-end 0)))))
            (setq cat (concat "[" cat "]"))
            (mhc-face-put cat 'mhc-calendar-hnf-face-cat)
            (setq cat (concat cat " ")))
           ;; NEW
           ((looking-at "^NEW[ \t]+\\(.*\\)$")
            (setq str (buffer-substring (match-beginning 1) (match-end 1)))
            (mhc-face-put str 'mhc-calendar-hnf-face-new)
            (setq new (format "%s%d" newmark ncount))
            (mhc-face-put new 'mhc-calendar-hnf-face-newtag)
            (setq str (concat "     " new " " cat str "\n"))
            (put-text-property 0 (length str) 'mhc-calendar/summary-hnf-prop count str)
            (setq summary (concat summary str)
                  count (1+ count)
                  ncount (1+ ncount)
                  cat ""))
           ;; LNEW
           ((looking-at "^LNEW[ \t]+\\([^ \t]+\\)[ \t]+\\(.*\\)$")
            (setq uri (concat "<"
                              (buffer-substring (match-beginning 1) (match-end 1))
                              ">"))
            (mhc-face-put uri 'mhc-calendar-hnf-face-uri)
            (setq str (buffer-substring (match-beginning 2) (match-end 2)))
            (mhc-face-put str 'mhc-calendar-hnf-face-new)
            (setq new (format "%s%d" newmark ncount))
            (mhc-face-put new 'mhc-calendar-hnf-face-newtag)
            (setq str (concat "     " new " " cat str " " uri "\n"))
            (put-text-property 0 (length str) 'mhc-calendar/summary-hnf-prop count str)
            (setq summary (concat summary str)
                  count (1+ count)
                  ncount (1+ ncount)
                  cat ""))
           ;; SUB
           ((looking-at "^SUB[ \t]+\\(.*\\)$")
            (setq str (buffer-substring (match-beginning 1) (match-end 1)))
            (mhc-face-put str 'mhc-calendar-hnf-face-sub)
            (setq str (concat "       " sub " " cat str "\n"))
            (put-text-property 0 (length str) 'mhc-calendar/summary-hnf-prop count str)
            (setq summary (concat summary str)
                  count (1+ count)
                  cat ""))
           ;; LSUB
           ((looking-at "^LSUB[ \t]+\\([^ \t]+\\)[ \t]+\\(.*\\)$")
            (setq uri (concat "<"
                              (buffer-substring (match-beginning 1) (match-end 1))
                              ">"))
            (mhc-face-put uri 'mhc-calendar-hnf-face-uri)
            (setq str (buffer-substring (match-beginning 2) (match-end 2)))
            (mhc-face-put str 'mhc-calendar-hnf-face-sub)
            (setq str (concat "       " sub " " cat str " " uri "\n"))
            (put-text-property 0 (length str) 'mhc-calendar/summary-hnf-prop count str)
            (setq summary (concat summary str)
                  count (1+ count)
                  cat "")))
          (forward-line)))
      (if summary (insert "\n" summary))
      (delete-char -1)
      (set-buffer-modified-p nil))))

(defun mhc-calendar-hnf-face-setup ()
  (interactive)
  (let ((ow (interactive-p)))
    (mhc-face-setup-internal mhc-calendar-hnf-face-alist ow)
    (mhc-face-setup-internal mhc-calendar-hnf-face-alist-internal nil)))

;;; Pseudo MUA Backend Methods:
(defun mhc-calendar-insert-summary-contents (inserter)
  (let ((beg (point))
        (name (or (mhc-record-name
                   (mhc-schedule-record mhc-tmp-schedule))
                  "Dummy")))
    (funcall inserter)
    (put-text-property beg (point) 'mhc-calendar/summary-prop name)
    (insert "\n")))

(provide 'mhc-calendar)
(put 'mhc-calendar 'insert-summary-contents 'mhc-calendar-insert-summary-contents)

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

;; mhc-calendar.el ends here
