;;; mhc-summary.el --- Summary major mode in MHC.

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Created: 2000/05/01
;; Revised: $Date$


;;; Commentary:

;; This file is a part of MHC.

;; This file consists of two parts: the first part contains MUA
;; backend functions, and the second part contains functions to make
;; summary contents.


;;; About MUA Backend:

;; In order to define new MUA backend, it is required to define these
;; methods.
;;
;;     (mhc-foo-summary-filename)
;;         Return the file name of the article on the current line in
;;         this summary buffer.
;;
;; Define these methods appropriately, and put definitions as follows:
;;
;;    (provide 'mhc-foo)
;;    (put 'mhc-foo 'summary-filename        'mhc-foo-summary-filename)

;;; Code:

(require 'mhc-face)
(require 'mhc-vars)
(require 'mhc-day)
(require 'mhc-compat)
(require 'mhc-schedule)
(require 'bytecomp)
(require 'mhc-record)
(require 'mhc-header)

;;; Global Variables:

(defcustom mhc-summary-language 'english
  "*Language of the summary."
  :group 'mhc
  :type '(choice (const :tag "English" english)
                 (const :tag "Japanese" japanese)))

(defcustom mhc-summary-use-cw nil
  "*If non-nil, insert `Calendar week number' instead of `Monday'."
  :group 'mhc
  :type '(choice (const :tag "Use" t)
                 (const :tag "No" nil)))

(defcustom mhc-use-week-separator t
  "*If non-nil insert separator in summary buffer."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-summary-separator ?-
  "*Character of the separator as 'mhc-use-week-separator'."
  :group 'mhc
  :type 'character)

(defcustom mhc-use-month-separator t
  "*Insert separator in summary buffer for wide scope."
  :group 'mhc
  :type '(choice (const :tag "Insert (full width)" t)
                 (integer :tag "Insert (number of width)")
                 (const :tag "Not use" nil)))

(defcustom mhc-summary-month-separator ?=
  "*Character of the separator as 'mhc-use-month-separator'."
  :group 'mhc
  :type 'character)

(defcustom mhc-summary-string-conflict "[C]"
  "*String which indicates conflicts in summary buffer."
  :group 'mhc
  :type 'string)

(defcustom mhc-summary-string-recurrence "[R]"
  "*String which indicates recurrences in summary buffer."
  :group 'mhc
  :type 'string)

(defcustom mhc-summary-string-secret "[SECRET]"
  "*String which hides private subjects in summary buffer."
  :group 'mhc
  :type 'string)

(defcustom mhc-use-icon t
  "*If non-nil, schedule icon is used."
  :group 'mhc
  :type 'boolean)

(defcustom mhc-icon-path (if (fboundp 'locate-data-directory)
                             (locate-data-directory "mhc"))
  "*Icon path for MHC."
  :group 'mhc
  :type 'directory)

(defcustom mhc-icon-setup-hook nil
  "*A hook called after icon setup."
  :group 'mhc
  :type 'hook)

(defcustom mhc-todo-string-done
  (if (eq mhc-summary-language 'japanese) "■" "[X]")
      "*String which indicates done TODO."
      :group 'mhc
      :type 'string)

(defcustom mhc-todo-string-not-done
  (if (eq mhc-summary-language 'japanese) "□" "[ ]")
      "*String which indicates not-done TODO."
      :group 'mhc
      :type 'string)

(defcustom mhc-summary-line-format
  (if (eq mhc-summary-language 'japanese)
      "%Y%年%M%月%D%日%(%曜%) %b%e %c%i%s %p%l"
    "%Y%/%M%/%D%S%W %b%e %c%r%i%s %p%l")
  "*A format string for summary line of MHC.
It may include any of the following format specifications
which are replaced by the given information:

%Y The year of the line if first line of the day.
%M The month of the line if first line of the day.
%D The day of the line if first line of the day.
%W The weekday name of the line if first line of the day.
%b Begin time.
%e End time (includes '-').
%c Warning string for conflict (See also `mhc-summary-string-conflict').
%i The icon for the schedule.
%s The subject of the schedule.
%p The priority of the schedule.
%l The location of the schedule.
%r Indicator for recurrence-tag (See also `mhc-summary-string-recurrence').
%t The indicator for TODO.

%/ A slash character if first line of the day.
%( A left parenthesis character if first line of the day.
%) A right parenthesis character if first line of the day.
%S A space with face.

%年 The '年' of the line if first line of the day.
%月 The '月' of the line if first line of the day.
%日 The '日' of the line if first line of the day.
%曜 The japaneses weekday name of the line if first line of the day.
"
  :group 'mhc
  :type 'string)

;;; Internal Variable:

(defconst mhc-summary-major-mode-alist
  '((mew-summary-mode  . mhc-mew)
    (mew-virtual-mode  . mhc-mew)
    (wl-folder-mode    . mhc-wl)
    (wl-summary-mode   . mhc-wl)
    (gnus-group-mode   . mhc-gnus)
    (gnus-summary-mode . mhc-gnus)))

;; Internal Variables which are bound while inserting line:
(defvar mhc-tmp-day-face nil "a face for the day.")
(defvar mhc-tmp-dayinfo  nil "a dayinfo for the day.")
(defvar mhc-tmp-schedule nil "a schedule structure.")
(defvar mhc-tmp-begin    nil "begin time.")
(defvar mhc-tmp-end      nil "end time.")
(defvar mhc-tmp-conflict nil "non-nil if conflicted schedule.")
(defvar mhc-tmp-recurrence nil "non-nil if recurrence schedule.")
(defvar mhc-tmp-first    nil "non-nil if first schedule.")
(defvar mhc-tmp-private  nil "non-nil if private display mode.")
(defvar mhc-tmp-priority nil "a priority of the schedule.")
;; For TODO.
(defvar mhc-tmp-day      nil "the day.")
(defvar mhc-tmp-deadline nil "a schedule structure.")

;; Inserter (internal variable)
(defvar mhc-summary/line-inserter nil)

(defvar mhc-summary-line-format-alist
  '((?Y (mhc-summary/line-year-string)
        'face mhc-tmp-day-face)
    (?/ (if mhc-tmp-first "/" " ")
        'face mhc-tmp-day-face)
    (?- (if mhc-tmp-first "-" " ")
        'face mhc-tmp-day-face)
    (?S " " 'face mhc-tmp-day-face)
    (?M (mhc-summary/line-month-string)
        'face mhc-tmp-day-face)
    (?D (mhc-summary/line-day-string)
        'face mhc-tmp-day-face)
    (?W (mhc-summary/line-day-of-week-string)
        'face mhc-tmp-day-face)
    (?b (if (null mhc-tmp-begin)
            (make-string 5 ? )
          (format "%02d:%02d" (/ mhc-tmp-begin 60) (% mhc-tmp-begin 60)))
        'face 'mhc-summary-face-time)
    (?e (if (or (null mhc-tmp-end)
                (and mhc-tmp-begin (= mhc-tmp-end mhc-tmp-begin)))
            (make-string 6 ? )
          (format "-%02d:%02d" (/ mhc-tmp-end 60) (% mhc-tmp-end 60)))
        'face 'mhc-summary-face-time)
    (?c (if mhc-tmp-conflict
            (if (and (mhc-use-icon-p) (mhc-icon-exists-p "conflict"))
                t
              mhc-summary-string-conflict))
        (if (and (mhc-use-icon-p) (mhc-icon-exists-p "conflict"))
            'icon 'face)
        (if (and (mhc-use-icon-p) (mhc-icon-exists-p "conflict"))
            (list "conflict") 'mhc-summary-face-conflict))
    (?r (if (and mhc-tmp-recurrence (not (string= "" mhc-tmp-recurrence)))
            (if (and (mhc-use-icon-p) (mhc-icon-exists-p "recurrence"))
                t
              mhc-summary-string-recurrence))
        (if (and (mhc-use-icon-p) (mhc-icon-exists-p "recurrence"))
            'icon 'face)
        (if (and (mhc-use-icon-p) (mhc-icon-exists-p "recurrence"))
            (list "recurrence") 'mhc-summary-face-recurrence))
    (?p (if mhc-tmp-priority
            (format "[%d]" mhc-tmp-priority))
        'face (cond
               ((null mhc-tmp-priority) nil)
               ((>= mhc-tmp-priority 80) 'mhc-summary-face-sunday)
               ((>= mhc-tmp-priority 50) 'mhc-summary-face-saturday)))
    (?i (not mhc-tmp-private) 'icon
        (if (mhc-schedule-in-category-p mhc-tmp-schedule "done")
            (delete "todo"
                    (copy-sequence (mhc-schedule-categories mhc-tmp-schedule)))
          (mhc-schedule-categories mhc-tmp-schedule)))
    (?s (mhc-summary/line-subject-string)
        'face
        (if mhc-tmp-private (mhc-face-category-to-face "Private")
          (mhc-face-category-to-face
           (car (mhc-schedule-categories mhc-tmp-schedule)))))
    (?t (cond
         ((mhc-schedule-in-category-p mhc-tmp-schedule "done") mhc-todo-string-done)
         ((mhc-schedule-in-category-p mhc-tmp-schedule "todo") mhc-todo-string-not-done))
        'face
        (cond
         ((mhc-schedule-in-category-p mhc-tmp-schedule "done") 'mhc-summary-face-done)
         ((mhc-schedule-in-category-p mhc-tmp-schedule "todo") 'mhc-summary-face-todo)))
    (?l (mhc-summary/line-location-string)
        'face 'mhc-summary-face-location)
    (?\( (if mhc-tmp-first "(" " ")
         'face mhc-tmp-day-face)
    (?\) (if mhc-tmp-first ")" " ")
         'face mhc-tmp-day-face)
    (?年 (if mhc-tmp-first "年" (make-string 2 ? ))
         'face mhc-tmp-day-face)
    (?月 (if mhc-tmp-first "月" (make-string 2 ? ))
         'face mhc-tmp-day-face)
    (?日 (if mhc-tmp-first "日" (make-string 2 ? ))
         'face mhc-tmp-day-face)
    (?曜 (mhc-summary/line-day-of-week-ja-string)
         'face mhc-tmp-day-face))
  "An alist of format specifications that can appear in summary lines.
Each element is a list of following:
\(SPEC STRING-EXP PROP-TYPE PROP-VALUE\)
SPEC is a character for format specification.
STRING is an expression to get string to insert.
PROP-TYPE is an expression to get one of the two symbols `face' or `icon'.
It indicates a type of the property to put on the inserted string.
PROP-VALUE is the property value correspond to PROP-TYPE.
")


(defvar mhc-summary/cw-separator nil)
(defvar mhc-summary/cw-week nil)

;;; MUA Backend Functions:

(defun mhc-summary/true (&rest args)
  "This is the dummy backend function, which always returns t."
  t)

(defsubst mhc-highlight-message (&optional for-draft)
  "Hilight message in the current buffer.
If optional argument FOR-DRAFT is non-nil, Hilight message as draft message."
  (set (make-local-variable 'font-lock-defaults)
       '(mhc-message-font-lock-keywords t)))

(defun mhc-summary-filename ()
  (let ((schedule) (filename))
    (if (and (setq schedule (get-text-property (point) 'mhc-schedule))
             (setq filename (mhc-record-name
                             (mhc-schedule-record schedule)))
             (file-exists-p filename)
             (not (file-directory-p filename)))
        filename
      nil)))

(defun mhc-summary-display-article ()
  "Display the current article pointed in summary."
  (let ((file (mhc-summary-filename)))
    (if (not (and (stringp file) (file-exists-p file)))
        (message "File does not exist.")
      (mhc-window-push)
      ;; (view-file-other-window file)
      (pop-to-buffer (get-buffer-create "*MHC message*"))
      ;; eword decode
      (let ((buffer-read-only nil))
        (goto-char (point-min))
        (erase-buffer)
        (mhc-insert-file-contents-as-coding-system
         mhc-default-coding-system file)
        (mhc-header-narrowing
          (mhc-header-delete-header
           "^\\(Content-.*\\|Mime-Version\\|User-Agent\\):" 'regexp))
        (mhc-header-delete-empty-header
         "^X-SC-.*:" 'regexp))
      ;; (setq view-exit-action 'mhc-calendar-view-exit-action)
      (set-visited-file-name nil)
      ;; (rename-buffer (file-name-nondirectory file) 'unique)
      ;; (run-hooks 'mhc-calendar-view-file-hook)
      (let ((buffer-read-only nil))
        (mhc-message-mode))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (mhc-message-set-file-name file)
      )))


(defun mhc-summary-get-import-buffer (&optional get-original)
  "Return a buffer visiting import article.
If GET-ORIGINAL is non-nil, return a cons of buffer: car keeps a raw
message and cdr keeps a visible message."
  (let ((buffer
         (or
          (save-window-excursion
            (let ((mode (progn (other-window 1) major-mode)))
              (if (or
                   (eq mode 'mew-message-mode)
                   (eq mode 'mhc-message-mode))
                  (current-buffer))))
          (current-buffer))))
    ;; XXX get-original is not effective now. gone soon.
    (if get-original
        (cons buffer buffer)
      buffer)))


(defun mhc-summary-generate-buffer (name-or-date)
  "Generate a summary buffer for DATE-OR-DATE, and change current buffer to it."
  (switch-to-buffer
   (set-buffer
    (mhc-get-buffer-create
     (if (stringp name-or-date)
         name-or-date
       (mhc-date-format name-or-date "%04d-%02d" yy mm)))))
  (setq inhibit-read-only t
        buffer-read-only nil
        indent-tabs-mode nil)
  (widen)
  (delete-region (point-min) (point-max)))

(defun mhc-summary-insert-contents (mhc-tmp-schedule
                                       mhc-tmp-private
                                       inserter
                                       &optional mailer)
  (let ((beg (point)))
    (if (eq 'direct mailer)
        (let ((mhc-use-icon nil))
          (mhc-summary-line-insert)
          (insert "\n"))
      (funcall inserter)
      (put-text-property beg (point) 'mhc-schedule mhc-tmp-schedule)
      (insert "\n")
      )))

(defsubst mhc-summary-search-date (date)
  "Search day in the current buffer."
  (let (dayinfo)
    (goto-char (point-min))
    (while (and (not (eobp))
                (or (null (setq dayinfo
                                (get-text-property (point) 'mhc-dayinfo)))
                    (not (eq (mhc-day-date dayinfo) date))))
      (goto-char (or (next-single-property-change (point) 'mhc-dayinfo)
                     (point-min))))))

(defun mhc-summary-record (&optional mailer)
  "Return record on current line."
  (let ((filename (mhc-summary-filename)))
    (if filename
        (mhc-parse-file filename))))

;;; Codes:
(defsubst mhc-summary/make-string (count character)
  (make-string (max 4 count) character))        ;; xxxx 4 ?


(defun mhc-summary/insert-separator (&optional char banner width)
  "Insert horizontal using CHAR in WIDTH.
CHAR is '-' if not specified. default WIDTH is calculated from window size.
If BANNER is set, it is printed on the horizontal line."
  (let ((hr (make-string (or width (- (mhc-misc-get-width) 2)) (or char ?-)))
        (bn (or banner ""))
        (bn-offset 4))
    (mhc-face-put hr 'mhc-summary-face-separator)
    (mhc-face-put bn 'mhc-summary-face-cw)
    (insert
     (concat
      (substring hr 0 bn-offset)
      bn
      (substring hr (+ bn-offset (length bn)) -1)
      "\n"))))

(defvar mhc-summary/today nil)

(defun mhc-summary/insert-dayinfo (mhc-tmp-dayinfo mailer category-predicate secret)
  (let ((time-max -1)
        (schedules (mhc-day-schedules mhc-tmp-dayinfo))
        (mhc-tmp-first t)
        mhc-tmp-begin mhc-tmp-end
        mhc-tmp-location mhc-tmp-schedule
        mhc-tmp-conflict mhc-tmp-recurrence mhc-tmp-priority
        next-begin displayed)
    (if schedules
        (progn
          (while schedules
            (if (funcall category-predicate (car schedules))
                (progn
                  (setq mhc-tmp-begin (mhc-schedule-time-begin (car schedules))
                        mhc-tmp-end (mhc-schedule-time-end (car schedules))
                        mhc-tmp-priority (mhc-schedule-priority
                                          (car schedules))
                        next-begin (if (car (cdr schedules))
                                       (mhc-schedule-time-begin
                                        (car (cdr schedules))))
                        mhc-tmp-conflict (or (and mhc-tmp-end next-begin
                                                  (< next-begin mhc-tmp-end))
                                             (and mhc-tmp-begin time-max
                                                  (< mhc-tmp-begin time-max)))
                        mhc-tmp-recurrence (mhc-schedule-recurrence-tag (car schedules)))
                  (if mhc-tmp-end (setq time-max (max mhc-tmp-end time-max)))
                  (setq displayed t)
                  (mhc-summary-insert-contents
                   (car schedules)
                   (and secret
                        (mhc-schedule-in-category-p
                         (car schedules) mhc-category-as-private))
                   'mhc-summary-line-insert
                   mailer)
                  (setq mhc-tmp-first nil)))
            (setq schedules (cdr schedules)))
          (if (not displayed)
              (mhc-summary-insert-contents nil secret
                                           'mhc-summary-line-insert
                                           mailer)))
      (mhc-summary-insert-contents nil secret
                                   'mhc-summary-line-insert
                                   mailer))))


(defun mhc-summary-make-contents
  (dayinfo-list &optional from to mailer category-predicate secret)
  (let* ((sparse (or from to))
         (from (or from (mhc-day-date (car dayinfo-list))))
         (to   (or to   (mhc-day-date (car (last dayinfo-list)))))
         (date from) dayinfo
         (separator-format (and mhc-summary-use-cw
                                mhc-use-week-separator
                                (eq mhc-start-day-of-week 1)
                                " CW %d ")))
    (while (mhc-date<= date to)
      (setq dayinfo (or (assoc date dayinfo-list)
                        (and sparse (mhc-day-new date))))
      (when dayinfo
        (mhc-summary/insert-dayinfo
         dayinfo
         (or mailer 'mhc-mua)
         (or category-predicate mhc-default-category-predicate-sexp)
         secret))
      (setq date (mhc-date++ date))
      ;; insert week separator
      (and sparse mhc-use-week-separator
           (eq (mhc-date-ww date) mhc-start-day-of-week)
           (mhc-summary/insert-separator
            mhc-summary-separator
            (and separator-format (format separator-format (mhc-date-cw date)))))
      ;; insert month separator
      (and sparse mhc-use-month-separator
           (eq (mhc-date-dd date) 1)
           (mhc-summary/insert-separator
            mhc-summary-month-separator)))))

(defun mhc-summary/line-year-string ()
  (if mhc-tmp-first
      (format "%4d" (mhc-day-year mhc-tmp-dayinfo))
    (make-string 4 ? )))


(defun mhc-summary/line-month-string ()
  (if mhc-tmp-first
      (format "%02d" (mhc-day-month mhc-tmp-dayinfo))
    (make-string 2 ? )))


(defun mhc-summary/line-day-string ()
  (if mhc-tmp-first
      (format "%02d" (mhc-day-day-of-month mhc-tmp-dayinfo))
    (make-string 2 ? )))


(defun mhc-summary/line-day-of-week-string ()
  (if mhc-tmp-first
      (let ((week (mhc-day-day-of-week mhc-tmp-dayinfo)))
        (if (and mhc-summary/cw-week (= week 1) )
            (format "%3s"
                    (format "w%d" (mhc-date-cw (mhc-day-date mhc-tmp-dayinfo))))
          (aref ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"] week)))
    (make-string 3 ? )))

(defun mhc-summary/line-day-of-week-ja-string ()
  (if mhc-tmp-first
      (let ((week (mhc-day-day-of-week mhc-tmp-dayinfo)))
        (if (and mhc-summary/cw-week(= week 1) )
            (format "%2d" (mhc-date-cw (mhc-day-date mhc-tmp-dayinfo)))
          (aref ["日" "月" "火" "水" "木" "金" "土"] week)))
    (make-string 2 ? )))


(defun mhc-summary/line-subject-string ()
  (if mhc-tmp-schedule
      (if mhc-tmp-private
          mhc-summary-string-secret
        (mhc-schedule-subject-as-string mhc-tmp-schedule))))

(defun mhc-summary/line-location-string ()
  (let ((location (mhc-schedule-location mhc-tmp-schedule)))
    (and (not mhc-tmp-private)
         location
         (> (length location) 0)
         (concat "[" location "]"))))


;;; Line format parsing

(defmacro mhc-line-insert (string)
  `(and (stringp ,string) (insert ,string)))

(defun mhc-line-parse-format (format spec-alist)
  (let ((f (mhc-string-to-char-list format))
        inserter entry)
    (setq inserter (list 'let (list 'pos)))
    (while f
      (if (eq (car f) ?%)
          (progn
            (setq f (cdr f))
            (if (eq (car f) ?%)
                (setq inserter (append inserter (list (list 'insert ?%))))
              (setq entry (assq (car f) spec-alist))
              (unless entry
                (error "Unknown format spec %%%c" (car f)))
              (setq inserter
                    (append inserter
                            (list (list 'setq 'pos (list 'point)))
                            (list (list 'mhc-line-insert
                                        (nth 1 entry)))
                            (and
                             (nth 2 entry)
                             (list
                              (append (cond
                                       ((eq (eval (nth 2 entry)) 'face)
                                        (list 'put-text-property
                                              'pos (list 'point)
                                              (list 'quote 'face)
                                              (nth 3 entry)))
                                       ((eq (eval (nth 2 entry)) 'icon)
                                        (list 'if
                                              (nth 1 entry)
                                              (list
                                               'and
                                               (list 'mhc-use-icon-p)
                                               (list 'mhc-put-icon
                                                     (nth 3 entry)))))))))))))
        (setq inserter (append inserter (list (list 'insert (car f))))))
      (setq f (cdr f)))
    inserter))


(defmacro mhc-line-inserter-setup (inserter format alist)
  `(let (byte-compile-warnings)
     (setq ,inserter
           (byte-compile
            (list 'lambda ()
		  (mhc-line-parse-format ,format ,alist))))
     (when (get-buffer "*Compile-Log*")
       (bury-buffer "*Compile-Log*"))
     (when (get-buffer "*Compile-Log-Show*")
       (bury-buffer "*Compile-Log-Show*"))))


(defun mhc-summary-line-inserter-setup ()
  "Setup MHC summary and line inserter."
  (interactive)
  (if (and (called-interactively-p 'interactive)
           (mhc-use-icon-p))
      (call-interactively 'mhc-icon-setup))
  (setq mhc-summary/cw-separator (and mhc-summary-use-cw
                                      mhc-use-week-separator
                                      (eq mhc-start-day-of-week 1)))
  (setq mhc-summary/cw-week (and mhc-summary-use-cw
                                 (not mhc-summary/cw-separator)))
  (mhc-line-inserter-setup
   mhc-summary/line-inserter
   mhc-summary-line-format
   mhc-summary-line-format-alist))


(defun mhc-summary-line-insert ()
  "Insert summary line."
  (let ((mhc-tmp-day-face (cond
                           ((mhc-day-holiday
                             mhc-tmp-dayinfo)
                            'mhc-category-face-holiday)
                           ((eq (mhc-day-day-of-week
                                 mhc-tmp-dayinfo) 0)
                            'mhc-summary-face-sunday)
                           ((eq (mhc-day-day-of-week mhc-tmp-dayinfo) 6)
                            'mhc-summary-face-saturday)
                           (t 'mhc-summary-face-default)))
        (pos (point)))
    (if (mhc-date= (mhc-day-date mhc-tmp-dayinfo) (mhc-date-now))
        (setq mhc-tmp-day-face (mhc-face-get-today-face mhc-tmp-day-face)))
    (funcall mhc-summary/line-inserter)
    (put-text-property pos (point) 'mhc-dayinfo mhc-tmp-dayinfo)))


(defvar mhc-summary-mode-map nil)

;; (unless mhc-summary-mode-map
  (setq mhc-summary-mode-map (make-sparse-keymap))
  (define-key mhc-summary-mode-map " " 'mhc-summary-scroll-message-forward)
  (define-key mhc-summary-mode-map (kbd "DEL") 'mhc-summary-scroll-message-backward)
  (define-key mhc-summary-mode-map "." 'mhc-summary-display)
  (define-key mhc-summary-mode-map "\C-m" 'mhc-summary-scroll-message-line-forward)
  (define-key mhc-summary-mode-map "v" 'mhc-summary-toggle-display-message)

  (define-key mhc-summary-mode-map "g" 'mhc-goto-month)
  (define-key mhc-summary-mode-map "/" 'mhc-search)
  (define-key mhc-summary-mode-map ">" 'mhc-goto-next-month)
  (define-key mhc-summary-mode-map "N" 'mhc-goto-next-year)
  (define-key mhc-summary-mode-map "<" 'mhc-goto-prev-month)
  (define-key mhc-summary-mode-map "P" 'mhc-goto-prev-year)

  (define-key mhc-summary-mode-map "s" 'mhc-rescan-month)
  (define-key mhc-summary-mode-map "D" 'mhc-delete)
  (define-key mhc-summary-mode-map "c" 'mhc-set-default-category)
  (define-key mhc-summary-mode-map "?" 'mhc-calendar)
  (define-key mhc-summary-mode-map "t" 'mhc-calendar-toggle-insert-rectangle)
  (define-key mhc-summary-mode-map "E" 'mhc-edit)
  (define-key mhc-summary-mode-map "M" 'mhc-modify)
  (define-key mhc-summary-mode-map "C" 'mhc-reuse-copy)
  (define-key mhc-summary-mode-map "R" 'mhc-reuse-past-event)
  (define-key mhc-summary-mode-map "Y" 'mhc-reuse-create)

  (define-key mhc-summary-mode-map "n" 'mhc-summary-display-next)
  (define-key mhc-summary-mode-map "p" 'mhc-summary-display-previous)
  (define-key mhc-summary-mode-map "f" 'forward-char)
  (define-key mhc-summary-mode-map "b" 'backward-char)

  (define-key mhc-summary-mode-map "j" 'mhc-summary-display-next)
  (define-key mhc-summary-mode-map "k" 'mhc-summary-display-previous)
  (define-key mhc-summary-mode-map "l" 'forward-char)
  (define-key mhc-summary-mode-map "h" 'backward-char)
;; )

(defun mhc-summary-mode ()
  "Major mode for MHC summary.

\\{mhc-summary-mode-map}"
  (interactive)
  (setq major-mode 'mhc-summary-mode
        mode-name  "MHC")
  (setq mode-line-buffer-identification (propertized-buffer-identification
                                         "MHC: %12b"))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t)
  (setq inhibit-read-only nil)
  (setq truncate-lines t)
  (use-local-map mhc-summary-mode-map)
  (run-hooks 'mhc-summary-mode-hook))

(defun mhc-summary-buffer-p (&optional buffer)
  (if buffer
      (set-buffer buffer))
  (eq major-mode 'mhc-summary-mode))

(defun mhc-summary-current-date (&optional p)
  (when (mhc-summary-buffer-p)
    (let* ((pos (or p (point)))
           (dayinfo (get-text-property pos 'mhc-dayinfo)))
      (or (and dayinfo (mhc-day-date dayinfo))
          (save-excursion
            (end-of-line)
            (while (and (>= pos (point-min))
                        (null dayinfo))
              (or (setq dayinfo (get-text-property pos 'mhc-dayinfo))
                  (setq pos (- pos 1))))
            (and dayinfo (mhc-day-date dayinfo)))))))

(defun mhc-summary-region-date ()
  (when (region-active-p)
    (let* ((p (region-beginning))
           dayinfo
           (datelist ()))
      (progn
        (while (<= p (region-end))
          (and (setq dayinfo (mhc-summary-current-date p))
               (setq datelist (cons dayinfo datelist)))
          (setq p (next-single-property-change p 'mhc-dayinfo)))
        (setq datelist (reverse (delete-dups datelist)))
        (if (< 1 (length datelist))
             (cons (car datelist) (car (last datelist)))
          (car datelist))))))

(defvar mhc-summary-buffer-current-date-month nil
  "Indicate summary buffer's month. It is also used by mhc-summary-buffer-p")
(make-variable-buffer-local 'mhc-summary-buffer-current-date-month)

(defun mhc-summary-current-date-month ()
  mhc-summary-buffer-current-date-month)
(defalias 'mhc-current-date-month 'mhc-summary-current-date-month)


(defun mhc-summary-display-message ()
  (interactive)
  (save-selected-window
    (mhc-summary-display-article)))

(defun mhc-summary-toggle-display-message ()
  (interactive)
  (if (mhc-message-visible-p)
      (mhc-message-delete-windows)
    (mhc-summary-display-message)))

(defvar mhc-message-file-name nil)
(make-variable-buffer-local 'mhc-message-file-name)

(defun mhc-message-set-file-name (file-name)
  (setq mhc-message-file-name file-name))

(defun mhc-message-visible-p (&optional file-name)
  "Return non-nil if MHC message is currently displaying, or nil if none."
  (and (get-buffer-window "*MHC message*")
       (or (null file-name)
           (save-selected-window
             (pop-to-buffer "*MHC message*")
             (and (stringp mhc-message-file-name)
                  (string= mhc-message-file-name file-name))))))

(defun mhc-message-delete-windows ()
  (delete-windows-on "*MHC message*"))

(defalias 'mhc-summary-display 'mhc-summary-display-message)

(defun mhc-summary-display-next ()
  (interactive)
  (forward-line)
  (if (mhc-message-visible-p)
      (mhc-summary-display)))

(defun mhc-summary-display-previous ()
  (interactive)
  (forward-line -1)
  (if (mhc-message-visible-p)
      (mhc-summary-display)))

(defun mhc-summary-scroll-message-line-forward ()
  (interactive)
  (mhc-summary-scroll-message-forward 1))

(defun mhc-summary-scroll-message-forward (&optional lines)
  (interactive)
  (mhc-summary-scroll-message 'forward lines))

(defun mhc-summary-scroll-message-backward (&optional lines)
  (interactive)
  (mhc-summary-scroll-message 'backward lines))

(defun mhc-summary-scroll-message (direction &optional lines)
  (interactive)
  (if (mhc-message-visible-p (mhc-summary-filename))
      (save-selected-window
        (pop-to-buffer "*MHC message*")
        (if (eq direction 'forward)
            (mhc-message-scroll-page-forward lines)
          (mhc-message-scroll-page-backward lines)))
    (mhc-summary-display-message)))

(provide 'mhc-summary)

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

;;; mhc-summary.el ends here
