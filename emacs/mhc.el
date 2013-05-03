;;; mhc.el -- MH Calendar.

;; Author:  Yoshinari Nomura <nom@quickhack.net>
;;
;; Created: 1994/07/04
;; Revised: $Date: 2009/05/31 12:54:50 $

;;;
;;; Commentay:
;;;

;; Mhc is the personal schedule management package cooperating
;;  with Mew, Wanderlust or Gnus.
;;
;; Minimum setup:
;;
;; for Mew user:
;;   (autoload 'mhc-mew-setup "mhc-mew")
;;   (add-hook 'mew-init-hook 'mhc-mew-setup)
;;; optional setting for Mew-1.94 (Raw JIS header decoding)
;;  (add-hook 'mew-message-hook 'mhc-mew-decode-header)
;;
;; for Wanderlust user:
;;   (autoload 'mhc-wl-setup "mhc-wl")
;;   (add-hook 'wl-init-hook 'mhc-wl-setup)
;;
;; for Gnus user:
;;   (autoload 'mhc-gnus-setup "mhc-gnus")
;;   (add-hook 'gnus-startup-hook 'mhc-gnus-setup)

(eval-when-compile (require 'cl))

;; For Mule 2.3
(eval-and-compile
  (when (boundp 'MULE)
    (require 'poe)
    (require 'pcustom)))

(require 'mhc-vars)
(require 'mhc-record)
(require 'mhc-file)
(require 'mhc-db)
(require 'mhc-misc)
(require 'mhc-date)
(require 'mhc-guess)
(require 'mhc-schedule)
(require 'mhc-face)
(require 'mhc-calendar)
(require 'mhc-draft)

(cond
 ((eval-when-compile  (and (not (featurep 'xemacs))
                           (>= emacs-major-version 21)
                           (if (eq system-type 'windows-nt)
                               ;; Meadow2 or NTEmacs21.3(and the later
                               ;; version) supports the image feature.
                               (or (featurep 'meadow)
                                   (>= emacs-major-version 22)
                                   (>= emacs-minor-version 3))
                             t)))
  (require 'mhc-e21))
 ((eval-when-compile
    (condition-case nil
        (require 'bitmap)
      (error nil)))
  (require 'mhc-bm))
 ((eval-when-compile (featurep 'xemacs))
  (require 'mhc-xmas))
 (t (defun mhc-use-icon-p ())))

(require 'mhc-minibuf)
(require 'mhc-summary)
(provide 'mhc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu setup
;;
(defvar mhc-mode-menu-spec
      '("Mhc"
        ["This month"   mhc-goto-this-month t]
        ["Next month"   mhc-goto-next-month t]
        ["Prev month"   mhc-goto-prev-month t]
        ["Goto month"   mhc-goto-month t]
        ["Goto date"    mhc-goto-date t]
        ["Import"       mhc-import t]
        ["Set category" mhc-set-default-category t]
        "----"
        ["Goto today"   mhc-goto-today (mhc-summary-buffer-p)]
        ["Modify"       mhc-modify (mhc-summary-buffer-p)]
        ["Edit"         mhc-edit (mhc-summary-buffer-p)]
        ["Rescan"       mhc-rescan-month (mhc-summary-buffer-p)]
        ["Delete"       mhc-delete (mhc-summary-buffer-p)]
        ["Insert Schedule" mhc-insert-schedule (not buffer-read-only)]
        ["3 months Mini calendar" mhc-calendar t]
        ["Toggle 3 months calendar" mhc-calendar-toggle-insert-rectangle
         (mhc-summary-buffer-p)]
        "----"
        ["Reset"        mhc-reset (mhc-summary-buffer-p)]
        ("Network"
         ["Online" mhc-file-toggle-offline mhc-file/offline]
         ["Offline" mhc-file-toggle-offline (not mhc-file/offline)]
         ["Sync" mhc-file-sync (and (not (and mhc-file/offline
                                              (not mhc-file-sync-enable-offline)))
                                    (if (eq mhc-file-method 'mhc-sync)
                                        (and (stringp mhc-sync-remote)
                                             (stringp mhc-sync-id))
                                      mhc-file-method))])
        "----"
        ("PostScript"
         ["PostScript" mhc-ps t]
         ["Preview" mhc-ps-preview t]
         ["Print" mhc-ps-print t]
         ["Save" mhc-ps-save t]
         ["Insert buffer" mhc-ps-insert-buffer t])))

(defvar mhc-prefix-key "\C-c."
  "*Prefix key to call MHC functions.")

(defvar mhc-mode-map nil "Keymap for `mhc-mode'.")
(defvar mhc-prefix-map nil "Keymap for 'mhc-key-prefix'.")

(if (and mhc-mode-map mhc-prefix-map)
    ()
  (setq mhc-mode-map (make-sparse-keymap))
  (setq mhc-prefix-map (make-sparse-keymap))
  (define-key mhc-prefix-map "g" 'mhc-goto-month)
  (define-key mhc-prefix-map "j" 'mhc-goto-date)
  (define-key mhc-prefix-map "." 'mhc-goto-this-month)
  (define-key mhc-prefix-map "n" 'mhc-goto-next-month)
  (define-key mhc-prefix-map "N" 'mhc-goto-next-year)
  (define-key mhc-prefix-map "p" 'mhc-goto-prev-month)
  (define-key mhc-prefix-map "P" 'mhc-goto-prev-year)
  (define-key mhc-prefix-map "f" 'mhc-goto-today)
  (define-key mhc-prefix-map "|" 'mhc-import)
  (define-key mhc-prefix-map "m" 'mhc-modify)
  (define-key mhc-prefix-map "e" 'mhc-edit)
  (define-key mhc-prefix-map "s" 'mhc-rescan-month)
  (define-key mhc-prefix-map "d" 'mhc-delete)
  (define-key mhc-prefix-map "c" 'mhc-set-default-category)
  (define-key mhc-prefix-map "i" 'mhc-insert-schedule)
  (define-key mhc-prefix-map "?" 'mhc-calendar)
  (define-key mhc-prefix-map "t" 'mhc-calendar-toggle-insert-rectangle)
  (define-key mhc-prefix-map "T" 'mhc-file-toggle-offline)
  (define-key mhc-prefix-map "S" 'mhc-file-sync)
  (define-key mhc-prefix-map "R" 'mhc-reset)
  (define-key mhc-prefix-map "@" 'mhc-todo-toggle-done)
  (define-key mhc-mode-map mhc-prefix-key mhc-prefix-map)
  (cond
   ((featurep 'xemacs)
    (define-key mhc-mode-map [(button1)] 'mhc-calendar-mouse-goto-date)
    (define-key mhc-mode-map [(button2)] 'mhc-calendar-mouse-goto-date-view))
   (t
    (define-key mhc-mode-map [mouse-1] 'mhc-calendar-mouse-goto-date)
    (define-key mhc-mode-map [mouse-2] 'mhc-calendar-mouse-goto-date-view))))

(defvar mhc-mode nil "Non-nil when in mhc-mode.")

(defcustom mhc-mode-hook nil
  "Hook run in when entering MHC mode."
  :group 'mhc
  :type 'hook)

;; Avoid warning of byte-compiler.
(defvar mhc-mode-menu)
(eval-and-compile
  (autoload 'easy-menu-add "easymenu"))

(defun mhc-mode (&optional arg) "\
\\<mhc-mode-map>
   MHC is the mode for registering schdule directly from email.
   Requres Mew or Wanderlust or Gnus.

   Key assinment on mhc-mode.

\\[mhc-goto-this-month] Review the schedule of this month
\\[mhc-goto-next-month] Review the schedule of next month
\\[mhc-goto-prev-month] Review the schedule of previous month
\\[mhc-goto-month]      Jump to your prefer month
\\[mhc-goto-date]       Jump to your prefer date
\\[mhc-rescan-month]    Rescan the buffer of the month
\\[mhc-goto-today]      Move cursor to today (Only available reviewing this month)
\\[mhc-import]  Register the reviewing mail to schdule
\\[mhc-delete]  Delete the schdule on the cursor line
\\[mhc-set-default-category]    Edit the schdule on the cursor line
\\[mhc-modify]  Modify the schdule on the cursor line
\\[mhc-edit]    Create new schdule file
\\[mhc-set-default-category]    Change default category
\\[mhc-calendar]        Display 3 months mini calendar
\\[mhc-calendar-toggle-insert-rectangle]        Toggle 3 months calendar
\\[mhc-reset]   Reset MHC

   '\\[universal-argument]' prefix is available on using '\\[mhc-rescan-month]', '\\[mhc-goto-this-month]', '\\[mhc-goto-month]', '\\[mhc-goto-date]'
  , it works to assign the category (see below).

   The prefix arg '\\[mhc-goto-next-month]', '\\[mhc-goto-prev-month]' is also available and you can indicate
   the number of months to forward/back.

   Field names using by MHC.

   X-SC-Category:
   Space-seperated Keywords. You can set default category to scan.
   You can also indicate keywords by typing '\\[mhc-rescan-month]', '\\[mhc-goto-this-month]', '\\[mhc-goto-month]', '\\[mhc-goto-date]' with C-u.
"
  (interactive "P")
  (make-local-variable 'mhc-mode)
  (setq mhc-mode
        (if (null arg)
            (not mhc-mode)
          (> (prefix-numeric-value arg) 0)))
  (when (featurep 'xemacs)
    (easy-menu-add mhc-mode-menu))
  (force-mode-line-update)
  (run-hooks 'mhc-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lexical analyzer part for category.
;;

(defsubst mhc-expr/new ()
  (vector nil nil nil nil))

(defsubst mhc-expr/token (expr-obj)        ;; literal
  (aref expr-obj 0))
(defsubst mhc-expr/token-type (expr-obj)   ;; symbolized
  (aref expr-obj 1))
(defsubst mhc-expr/string (expr-obj)       ;; currently parsing string.
  (aref expr-obj 2))

(defsubst mhc-expr/set-token (expr-obj val)
  (aset expr-obj 0 val))
(defsubst mhc-expr/set-token-type (expr-obj val)
  (aset expr-obj 1 val))
(defsubst mhc-expr/set-string (expr-obj val)
  (aset expr-obj 2 val))

(defconst mhc-expr-token-type-alist
  '(
    ("[^!&|()\t \n]+"  . symbol)
    ("!"              . negop)
    ("&&"             . andop)
    ("||"             . orop)
    ("("              . lparen)
    (")"              . rparen)))

;; Eat one token from parsing string in obj.
(defun mhc-expr/gettoken (obj)
  (let ((string (mhc-expr/string obj))
        (token-alist mhc-expr-token-type-alist)
        (token-type nil)
        (token      nil))
    ;; delete leading white spaces.
    (if (string-match "^[\t ]+" string)
        (setq string (substring string (match-end 0))))
    (while (and token-alist (not token-type))
      (if (string-match (concat "^" (car (car token-alist))) string)
          (setq token      (substring string 0 (match-end 0))
                string     (substring string (match-end 0))
                token-type (cdr (car token-alist))))
      (setq token-alist (cdr token-alist)))

    (mhc-expr/set-token      obj token)
    (mhc-expr/set-string     obj string)
    (mhc-expr/set-token-type obj token-type)
    obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recursive descent parser for category.
;;

;;
;; expression -> term ("||" term)*
;;
(defun mhc-expr/expression (obj)
  (let ((ret (list (mhc-expr/term obj))))
    (while (eq (mhc-expr/token-type obj) 'orop)
      (mhc-expr/gettoken obj)
      (setq ret (cons (mhc-expr/term obj) ret)))
    (if (= 1 (length ret))
        (car ret)
      (cons 'or (nreverse ret)))))

;;
;; term       -> factor ("&&" factor)*
;;
(defun mhc-expr/term (obj)
  (let ((ret (list (mhc-expr/factor obj))))
    (while (eq (mhc-expr/token-type obj) 'andop)
      (mhc-expr/gettoken obj)
      (setq ret (cons (mhc-expr/factor obj) ret)))
    (if (= 1 (length ret))
        (car ret)
      (cons 'and (nreverse ret)))))

;;
;; factor     -> "!"* category_name || "(" expression ")"
;;
(defun mhc-expr/factor (obj)
  (let ((ret)
        (neg-flag nil))
    (while (eq (mhc-expr/token-type obj) 'negop)
      (setq neg-flag (not neg-flag))
      (mhc-expr/gettoken obj))
    (cond
     ;; symbol
     ((eq (mhc-expr/token-type obj) 'symbol)
      (setq ret (list 'mhc-schedule-in-category-p
                      'schedule (mhc-expr/token obj)))
      (mhc-expr/gettoken obj))
     ;; ( expression )
     ((eq (mhc-expr/token-type obj) 'lparen)
      (mhc-expr/gettoken obj)
      (setq ret (mhc-expr/expression obj))
      (if (not (eq (mhc-expr/token-type obj) 'rparen))
          (error "Syntax error."))
      (mhc-expr/gettoken obj))
     ;; error
     (t
      (error "Syntax error.")
      ;; (error "Missing category name or `(' %s %s"
      ;;  mhc-expr-token mhc-expr-parsing-string)
      ))
    (if neg-flag (list 'not ret) ret)))

(defun mhc-expr-parse (string)
  (let ((obj (mhc-expr/new)) (ret nil))
    (if (or (not string) (string= string ""))
        t
      (mhc-expr/set-string obj string)
      (mhc-expr/gettoken obj)
      (setq ret (mhc-expr/expression obj))
      (if (mhc-expr/token obj)
          (error "Syntax Error.")
        ret))))

(defun mhc-expr-compile (string)
  (byte-compile
   `(lambda (schedule)
      ,(mhc-expr-parse string)
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; category
;;
(defvar mhc-default-category nil)
(defvar mhc-default-category-predicate-sexp
  (mhc-expr-compile ""))

(defvar mhc-default-category-hist nil)

(defun mhc-set-default-category ()
  (interactive)
  (setq mhc-default-category
        (read-from-minibuffer "Default Category: "
                              (or mhc-default-category "")
                              nil nil 'mhc-default-category-hist))
  (setq mhc-default-category-predicate-sexp
        (mhc-expr-compile mhc-default-category))
  (if (mhc-summary-buffer-p)
      (mhc-rescan-month)))

; (defun mhc-category-convert (lst)
;   (let (ret inv)
;     ;; preceding `!' means invert logic.
;     (if (and lst (string-match "^!" (car lst)))
;       (setq lst (cons (substring (car lst) (match-end 0)) (cdr lst))
;             inv t))
;     (cons inv lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; goto-*

(defun mhc-goto-month (&optional date hide-private)
  "*Show schedules of specified month.
If HIDE-PRIVATE, priavate schedules are suppressed."
  (interactive
   (list
    (mhc-input-month "Month ")
    (if mhc-default-hide-private-schedules
        (not current-prefix-arg)
      current-prefix-arg)))
  (mhc-scan-month date
                  (mhc-summary-mailer-type)
                  mhc-default-category-predicate-sexp
                  hide-private))

(defvar mhc-goto-date-func 'mhc-goto-date-calendar)
                                        ; or mhc-goto-date-summary
(defun mhc-goto-date (&optional hide-private)
  "*Show schedules of specified date.
If HIDE-PRIVATE, private schedules are suppressed."
  (interactive
   (list
    (if mhc-default-hide-private-schedules
        (not current-prefix-arg)
      current-prefix-arg)))
  (let* ((owin (get-buffer-window (current-buffer)))
         (buf (mhc-summary-get-import-buffer))
         (win (if buf (get-buffer-window buf) nil))
         date)
    (save-excursion
      (when win (select-window win))
      (setq date (car (mhc-input-day "Date: " (mhc-date-now) (mhc-guess-date))))
      (select-window owin))
    (funcall mhc-goto-date-func date hide-private)))
(defun mhc-goto-date-calendar (date hide-private)
  (mhc-calendar-goto-month date))
(defun mhc-goto-date-summary (date hide-private)
  ;; XXX mhc-calendar-scanのパクリです
  (mhc-goto-month date hide-private)
  (goto-char (point-min))
  (if (mhc-summary-search-date date)
      (progn
        (beginning-of-line)
        (if (not (pos-visible-in-window-p (point)))
            (recenter)))))

(defun mhc-goto-this-month (&optional hide-private)
  "*Show schedules of this month.
If HIDE-PRIVATE, private schedules are suppressed."
  (interactive
   (list
    (if mhc-default-hide-private-schedules
        (not current-prefix-arg)
      current-prefix-arg)))
  (mhc-goto-month (mhc-date-now) hide-private))

(defun mhc-goto-next-month (&optional arg)
  (interactive "p")
  (mhc-goto-month (mhc-date-mm+
                   (or (mhc-current-date-month) (mhc-date-now)) arg)
                  mhc-default-hide-private-schedules))

(defun mhc-goto-next-year (&optional arg)
  (interactive "p")
  (mhc-goto-next-month (* (or arg 1) 12)))

(defun mhc-goto-prev-month (&optional arg)
  (interactive "p")
  (mhc-goto-next-month (- arg)))

(defun mhc-goto-prev-year (&optional arg)
  (interactive "p")
  (mhc-goto-next-year (- arg)))

(defun mhc-goto-today (&optional no-display)
  "*Go to the line of today's schedule or first day of month.
Unless NO-DISPLAY, display it."
  (interactive "P")
  (let ((now (mhc-date-now))
        (buf-date (mhc-current-date-month)))
    (when buf-date
      (goto-char (point-min))
      (mhc-date-let now
        (if (and (= yy (mhc-date-yy buf-date))
                 (= mm (mhc-date-mm buf-date)))
            (when (mhc-summary-search-date now)
              (forward-line 0)
              (or (pos-visible-in-window-p (point))
                  (recenter))
              (or no-display
                  (mhc-summary-display-article)))
          (when (and mhc-use-wide-scope
                     (mhc-summary-search-date (mhc-date-mm-first buf-date)))
            (forward-line 0)
            (or (pos-visible-in-window-p (point))
                (recenter))
            (or no-display
                (mhc-summary-display-article)))))
      ;; Emacs-21.3.50 something wrong
      (beginning-of-line))))

(defun mhc-rescan-month (&optional hide-private)
  "*Rescan schedules of this buffer.
If HIDE-PRIVATE, private schedules are suppressed."
  (interactive
   (list
    (if mhc-default-hide-private-schedules
        (not current-prefix-arg)
      current-prefix-arg)))
  (move-to-column 1)
  (let ((line (+ (count-lines (point-min) (point))
                 (if (= (current-column) 0) 1 0))))
    (mhc-scan-month (or (mhc-current-date-month) (mhc-date-now))
                    (mhc-summary-mailer-type)
                    mhc-default-category-predicate-sexp
                    hide-private)
    (goto-line line)
    (beginning-of-line)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make scan form.

(defvar mhc-face-week-color-paint-thick nil)

(defvar mhc-summary-buffer-current-date-month nil
  "Indicate summary buffer's month. It is also used by mhc-summary-buffer-p")
(make-variable-buffer-local 'mhc-summary-buffer-current-date-month)


(defun mhc-scan-month (date mailer category-predicate secret)
  (let ((from  (mhc-date-mm-first date))
        (to    (mhc-date-mm-last date))
        (today (mhc-date-now))
        bfrom bto afrom ato wweek0 wweek1 wweek2)
    (or (eq 'direct mailer)
        (mhc-summary-generate-buffer date mailer))
    (when mhc-use-wide-scope
      (if (and mhc-use-week-separator
               (not (eq (mhc-end-day-of-week) 0)))
          (setq wweek0 0 wweek1 6 wweek2 7)
        (setq wweek0 1 wweek1 0 wweek2 8))
      (cond
       ((integerp mhc-use-wide-scope)
        (setq bfrom (mhc-date- from mhc-use-wide-scope))
        (setq bto (mhc-date-mm-last bfrom))
        (setq ato (mhc-date+ to mhc-use-wide-scope))
        (setq afrom (mhc-date-mm-first ato)))
       ((eq mhc-use-wide-scope 'week)
        (if (eq (mhc-date-ww from) wweek0)
            (setq bfrom nil bto nil)
          (setq bfrom
                (mhc-date+ (mhc-date- from 7)
                           (% (mhc-date- wweek2 (mhc-date-ww from)) 7)))
          (setq bto (mhc-date-mm-last bfrom)))
        (if (eq (mhc-date-ww to) wweek1)
            (setq afrom nil ato nil)
          (setq ato (mhc-date+ to (mhc-date- wweek2 (mhc-date-ww to) 1)))
          (setq afrom (mhc-date-mm-first ato))))
       ((eq mhc-use-wide-scope 'wide)
        (if (eq (mhc-date-ww from) wweek0)
            (setq bfrom (mhc-date- from 7))
          (setq bfrom
                (mhc-date+ (mhc-date- from 7)
                           (% (mhc-date- wweek2 (mhc-date-ww from)) 7))))
        (setq bto (mhc-date-mm-last bfrom))
        (if (eq (mhc-date-ww to) wweek1)
            (setq ato (mhc-date+ to 7))
          (setq ato (mhc-date+ to (mhc-date- wweek2 (mhc-date-ww to) 1))))
        (setq afrom (mhc-date-mm-first ato)))))
    (message "%s" (mhc-date-format date "Scanning %04d/%02d..." yy mm))
    (unless (eq 'direct mailer)
      (when (and (eq mhc-todo-position 'top)
                 (or mhc-insert-todo-list mhc-insert-memo-list))
        (mhc-summary-make-todo-memo today mailer category-predicate secret)
        (insert (make-string mhc-todo-mergin ?\n))
        (mhc-summary/insert-separator))
      (setq mhc-summary-buffer-current-date-month
            (mhc-date-mm-first date)))
    (when (and bfrom bto)
      (mhc-summary-make-contents bfrom bto mailer category-predicate secret)
      (if mhc-use-month-separator
          (mhc-summary/insert-separator
           'wide
           (when (eq (mhc-end-day-of-week) (mhc-date-ww bto))
             (if mhc-summary/cw-separator
                 (format " CW %d " (mhc-date-cw (mhc-date++ bto)))
               (make-string (length " CW 00 ") mhc-summary-month-separator))))
        (if (and mhc-use-week-separator
                 (eq (mhc-end-day-of-week) (mhc-date-ww bto)))
            (mhc-summary/insert-separator
             nil
             (when mhc-summary/cw-separator
               (format " CW %d " (mhc-date-cw (mhc-date++ bto))))))))
    (mhc-summary-make-contents from to mailer category-predicate secret)
    (when (and afrom ato)
      (if mhc-use-month-separator
          (mhc-summary/insert-separator
           'wide
           (when (eq mhc-start-day-of-week (mhc-date-ww afrom))
             (if mhc-summary/cw-separator
                 (format " CW %d " (mhc-date-cw afrom))
               (make-string (length " CW 00 ") mhc-summary-month-separator))))
        (if (and mhc-use-week-separator
                 (eq mhc-start-day-of-week (mhc-date-ww afrom)))
            (mhc-summary/insert-separator
             nil
             (when mhc-summary/cw-separator
                   (format " CW %d " (mhc-date-cw afrom))))))
      (mhc-summary-make-contents afrom ato mailer category-predicate secret))
    (unless (eq 'direct mailer)
      (when (and (eq mhc-todo-position 'bottom)
                 (or mhc-insert-todo-list mhc-insert-memo-list))
        (mhc-summary/insert-separator)
        (insert (make-string mhc-todo-mergin ?\n))
        (mhc-summary-make-todo-memo today mailer category-predicate secret))
      (when mhc-insert-calendar
        (mhc-calendar-insert-rectangle-at
         date
         (- (mhc-misc-get-width) mhc-calendar-width)
         mhc-vertical-calendar-length))
      (mhc-summary-mode-setup date mailer)
      (mhc-mode 1)
      (setq inhibit-read-only nil)
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (setq mhc-summary-buffer-current-date-month
            (mhc-date-mm-first date))
      (mhc-goto-today t)
      (message "%s" (mhc-date-format date "Scanning %04d/%02d...done" yy mm)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; import, edit, delete, modify

(defcustom mhc-input-sequences '(date time subject location category recurrence-tag alarm)
  "*Sequence of the inputs."
  :group 'mhc
  :type '(repeat (choice (const :tag "Date" date)
                         (const :tag "Time" time)
                         (const :tag "Subject" subject)
                         (const :tag "Location" location)
                         (const :tag "Category" category)
                         (const :tag "Recurrence tag" recurrence-tag)
                         (const :tag "Alarm" alarm))))

(defun mhc-edit (&optional import-buffer)
  "Edit a new schedule.
If optional argument IMPORT-BUFFER is specified, import its content.
Returns t if the importation was succeeded."
  (interactive
   (if current-prefix-arg
       (list (get-buffer (read-buffer "Import buffer: "
                                      (current-buffer))))))
  (let ((draft-buffer (generate-new-buffer mhc-draft-buffer-name))
        (current-date (or (mhc-current-date) (mhc-calendar-get-date) (mhc-date-now)))
        (succeed t)
        msgp date time subject location category recurrence-tag priority alarm)
    (and (interactive-p)
         (mhc-window-push))
    (set-buffer draft-buffer)
    (if import-buffer
        (progn
          (insert-buffer (if (consp import-buffer)
                             (cdr import-buffer)
                           import-buffer))
          (mhc-header-narrowing
            (setq msgp (or (mhc-header-get-value "from")
                           (mhc-header-get-value "x-sc-subject")))
            (mhc-header-delete-header
             (concat "^\\("
                     (mhc-regexp-opt mhc-draft-unuse-hdr-list)
                     "\\)")
             'regexp))
          (mhc-highlight-message)
          (switch-to-buffer draft-buffer t)))
    (condition-case ()
        (if import-buffer
            (progn
              (delete-other-windows)
              (if (y-or-n-p "Do you want to import this article? ")
                  (let* ((original (save-excursion
                                     (set-buffer
                                      (if (consp import-buffer)
                                          (cdr import-buffer)
                                        import-buffer))
                                     (mhc-parse-buffer)))
                         (schedule (car (mhc-record-schedules original)))
                         (inputs (copy-sequence mhc-input-sequences))
                         input)
                    (while (setq input (car inputs))
                      (setq inputs (delq input inputs))
                      (cond
                       ((eq input 'date)
                        ;; input date
                        (setq date
                              (mhc-input-day "Date: "
                                             current-date
                                             (mhc-guess-date))))
                       ((eq input 'time)
                        ;; input time
                        (setq time
                              (mhc-input-time "Time: "
                                              (mhc-schedule-time-as-string
                                               schedule)
                                              (mhc-guess-time
                                               (mhc-minibuf-candidate-nth-begin)))))
                       ((eq input 'subject)
                        ;; input subject
                        (setq subject
                              (mhc-input-subject
                               "Subject: "
                               (mhc-misc-sub
                                (or (mhc-record-subject original)
                                    (mhc-header-narrowing
                                      (mhc-header-get-value "subject")))
                                "^\\(Re:\\)? *\\(\\[[^\]]+\\]\\)? *"
                                ""))))
                       ((eq input 'location)
                        ;; input location
                        (setq location
                              (mhc-input-location
                               "Location: "
                               (mhc-schedule-location schedule))))
                       ((eq input 'category)
                        ;; input category
                        (setq category
                              (mhc-input-category
                               "Category: "
                               (mhc-schedule-categories-as-string schedule))))
                        ;; input recurrence tag
                       ((eq input 'recurrence-tag)
                        (setq recurrence-tag
                              (mhc-input-recurrence-tag
                               "Recurrence Tag: "
                               (mhc-schedule-recurrence-tag-as-string schedule))))
                       ;; input alarm
                       ((eq input 'alarm)
                        (if mhc-ask-alarm
                            (setq alarm
                                  (mhc-input-alarm
                                   "Alarm: "
                                   mhc-default-alarm))))))
                    ;;
                    (setq priority (mhc-schedule-priority schedule)))
                ;; Answer was no.
                (message "") ; flush minibuffer.
                (and (interactive-p)
                     (mhc-window-pop))
                (setq succeed nil)
                (kill-buffer draft-buffer)))
          ;; No import (it succeeds).
          (let ((inputs (copy-sequence mhc-input-sequences))
                input)
            (while (setq input (car inputs))
              (setq inputs (delq input inputs))
              (cond
               ((eq input 'date)
                (setq date (mhc-input-day "Date: " current-date)))
               ((eq input 'time)
                (setq time (mhc-input-time "Time: ")))
               ((eq input 'subject)
                (setq subject (mhc-input-subject "Subject: ")))
               ((eq input 'location)
                (setq location (mhc-input-location "Location: ")))
               ((eq input 'category)
                (setq category (mhc-input-category "Category: ")))
               ((eq input 'recurrence-tag)
                (setq recurrence-tag (mhc-input-recurrence-tag "Recurrence Tag: " (or subject ""))))
               ((eq input 'alarm)
                (if mhc-ask-alarm
                    (setq alarm (mhc-input-alarm "Alarm: " mhc-default-alarm))))))))
      ;; Quit.
      (quit
       (and (interactive-p)
            (mhc-window-pop))
       (setq succeed nil)
       (kill-buffer draft-buffer)))
    (if succeed
        (progn
          (switch-to-buffer draft-buffer t)
          (set-buffer draft-buffer)
          (if (and import-buffer msgp)
              (if (consp import-buffer)
                  (mhc-draft-reedit-buffer (car import-buffer) 'original)
                ;; Delete candidate overlay if exists.
                (if mhc-minibuf-candidate-overlay
                    (delete-overlay mhc-minibuf-candidate-overlay))
                ;; Already imported to current buffer.
                (mhc-draft-reedit-buffer (current-buffer)))
            ;; Delete candidate overlay if exists.
            (if mhc-minibuf-candidate-overlay
                (delete-overlay mhc-minibuf-candidate-overlay))
            (mhc-draft-setup-new))
          (mhc-header-narrowing
            (mhc-header-delete-header
             (concat "^\\("
                     (mhc-regexp-opt (mhc-header-list))
                     "\\)")
             'regexp))
          (goto-char (point-min))
          (insert "X-SC-Subject: " subject
                  "\nX-SC-Location: " location
                  "\nX-SC-Day: "
                  (mapconcat
                   (lambda (day)
                     (mhc-date-format day "%04d%02d%02d" yy mm dd))
                   date " ")
                  "\nX-SC-Time: "
                  (if time
                      (let ((begin (car time))
                            (end (nth 1 time)))
                        (concat
                         (if begin (mhc-time-to-string begin) "")
                         (if end (concat "-" (mhc-time-to-string end)) "")))
                    "")
                  "\nX-SC-Category: "
                  (mapconcat (function capitalize) category " ")
                  "\nX-SC-Priority: " (if priority
                                          (number-to-string priority)
                                        "")
                  "\nX-SC-Recurrence-Tag: " recurrence-tag
                  "\nX-SC-Cond: "
                  "\nX-SC-Duration: "
                  "\nX-SC-Alarm: " (or alarm "")
                  "\nX-SC-Record-Id: " (mhc-record-create-id) "\n")
          (goto-char (point-min))
          (mhc-draft-mode)
          succeed))))

(defcustom mhc-default-import-original-article nil
  "*If non-nil value, import a schedule with MIME attachements."
  :group 'mhc
  :type 'boolean)

(defun mhc-import (&optional get-original)
  "Import a schedule from the current article.
The default action of this command is to import a schedule from the
current article without MIME attachements.  If you want to import a
schedule including MIME attachements, call this command with a prefix
argument.  Set non-nil to `mhc-default-import-original-article', and
the default action of this command is changed to the latter."
  (interactive
   (list (if mhc-default-import-original-article
             (not current-prefix-arg)
           current-prefix-arg)))
  (mhc-window-push)
  (unless (mhc-edit (mhc-summary-get-import-buffer get-original))
    ;; failed.
    (mhc-window-pop)))

(defun mhc-import-from-region (s e)
  "Import a schedule from region."
  (interactive "r")
  (save-restriction
    (narrow-to-region s e)
    (let ((str (buffer-substring s e)))
      (mhc-import)
      (goto-char (point-max))
      (insert str)
      (goto-char (point-min)))))

(defun mhc-delete ()
  "Delete the current schedule."
  (interactive)
  (mhc-delete-file (mhc-summary-record)))

(defcustom mhc-delete-file-hook nil
  "Normal hook run after mhc-delete-file."
  :group 'mhc
  :type 'hook)

(defun mhc-delete-file (record)
  (interactive)
  (if (not (and record (file-exists-p (mhc-record-name record))))
      (message "File does not exist (%s)." (mhc-record-name record))
    (if (not (y-or-n-p (format "Do you delete %s ?"
                               (mhc-record-subject-as-string record))))
        (message "Never mind..")
      (if (and
           (mhc-record-occur-multiple-p record)
           (not (y-or-n-p
                 (format
                  "%s has multiple occurrences. Delete all(=y) or one(=n) ?"
                  (mhc-record-subject-as-string record)))))
          (mhc-db-add-exception-rule
           record
           (or (mhc-current-date)
               (mhc-calendar-view-date)))
        (mhc-db-delete-file record))
      (or (and (mhc-summary-buffer-p)
               (mhc-rescan-month mhc-default-hide-private-schedules))
          (and (mhc-calendar-p) (mhc-calendar-rescan)))
      (run-hooks 'mhc-delete-file-hook))))

(defun mhc-modify ()
  "Modify the current schedule."
  (interactive)
  (mhc-modify-file (mhc-summary-filename)))

(defun mhc-todo-set-as-done ()
  "Set TODO as DONE."
  (interactive)
  (mhc-modify-file (mhc-summary-filename))
  (mhc-draft-set-as-done)
  (mhc-draft-finish)
  (message ""))

(defun mhc-todo-set-as-not-done ()
  "Set TODO as NOT-DONE."
  (interactive)
  (mhc-modify-file (mhc-summary-filename))
  (mhc-draft-set-as-not-done)
  (mhc-draft-finish)
  (message ""))

(defun mhc-todo-toggle-done ()
  "Toggle between done and not for todo"
  (interactive)
  (mhc-modify-file (mhc-summary-filename))
  (mhc-draft-toggle-done)
  (mhc-draft-finish)
  (message ""))

(defcustom mhc-browse-x-url-function 'browse-url
  "*A function to browse URL."
  :group 'mhc
  :type 'function)

(defun mhc-browse-x-url ()
  "Browse X-URL field."
  (interactive)
  (let ((filename (mhc-summary-filename))
        url)
    (with-temp-buffer
      (insert-file-contents filename)
      (if (setq url (mhc-header-narrowing
                      (or (mhc-header-get-value "x-uri")
                          (mhc-header-get-value "x-url"))))
          (progn
            (funcall mhc-browse-x-url-function url)
            (message "X-URL browser started."))
        (message "No X-URL field.")))))

(defun mhc-modify-file (file)
  (if (and (stringp file) (file-exists-p file))
      (let* ((name (format
                    "*mhc draft %s/%s*"
                    mhc-base-folder
                    (file-relative-name
                     file
                     (file-name-as-directory
                      (mhc-summary-folder-to-path mhc-base-folder)))))
             (buffer (get-buffer name)))
        (if (buffer-live-p buffer)
            (progn
              (message "Specified file(%s) has already been opened." file)
              (switch-to-buffer-other-window buffer))
          (mhc-window-push)
          (set-buffer (setq buffer (get-buffer-create name)))
          (mhc-draft-reedit-file file)
          (set-buffer-modified-p nil)
          (switch-to-buffer-other-window buffer)
          (goto-char (point-min))
          (mhc-draft-mode)
          (set (make-local-variable 'mhc-draft-buffer-file-name) file)))
    (message "Specified file(%s) does not exist." file)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Window stack
;;

(defvar mhc-window-stack       nil)

(defun mhc-window-push ()
  (interactive)
  (setq mhc-window-stack
        (cons (current-window-configuration) mhc-window-stack)))

(defun mhc-window-pop ()
  (interactive)
  (if mhc-window-stack
      (set-window-configuration (car-safe mhc-window-stack)))
  (setq mhc-window-stack (cdr-safe mhc-window-stack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (Category . (parent-face fg bg))
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; manipulate data from mhc-summary-buffer.

(defconst mhc-summary-day-regex  "\\([^|]+| +\\)?[0-9]+/\\([0-9]+\\)")
(defconst mhc-summary-buf-regex
  (concat mhc-base-folder "/\\([0-9]+\\)/\\([0-9]+\\)"))

;(defun mhc-summary-buffer-p (&optional buffer)
;  (string-match mhc-summary-buf-regex
;               (buffer-name
;                (or buffer (current-buffer)))))

(defun mhc-summary-buffer-p (&optional buffer)
  (if buffer
      (set-buffer buffer))
  mhc-summary-buffer-current-date-month)

(defun mhc-current-date ()
  (when (mhc-summary-buffer-p)
    (let ((dayinfo (get-text-property (point) 'mhc-dayinfo)))
      (or (and dayinfo (mhc-day-date dayinfo))
          (save-excursion
            (end-of-line)
            (while (and (not (bobp))
                        (null dayinfo))
              (or (setq dayinfo (get-text-property (point) 'mhc-dayinfo))
                  (forward-char -1)))
            (and dayinfo (mhc-day-date dayinfo)))))))

; (defun mhc-current-date-month ()
;   (let ((buf (buffer-name)) yy mm dd)
;     (if (not (string-match mhc-summary-buf-regex buf))
;       nil
;       (mhc-date-new (string-to-number (match-string 1 buf))
;                   (string-to-number (match-string 2 buf))
;                   1))))

(defun mhc-current-date-month ()
  mhc-summary-buffer-current-date-month)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc.

;;
;; Convinient function when you want to insert your schedule into an
;; editing buffer.
;;
(defun mhc-insert-schedule (&optional hide-private)
  (interactive "P")
  (set-mark (point))
  (mhc-scan-month (mhc-input-month "Month ")
                  'direct ;; insert into current buffer.
                  mhc-default-category-predicate-sexp
                  hide-private)
  (exchange-point-and-mark))

(defun mhc-view-file ()
  "View the schedule on the current line in View mode in another window."
  (interactive)
  (let ((path (mhc-summary-filename)))
    (view-file-other-window path)))


;;; Temporary buffers

(defvar mhc-tmp-buffer-list nil)

(defun mhc-get-buffer-create (name)
  "Return buffer for temporary use of MHC."
  (let ((buf (get-buffer name)))
    (or (and buf (buffer-name buf))
        (progn
          (setq buf (get-buffer-create name)
                mhc-tmp-buffer-list (cons buf mhc-tmp-buffer-list))
          (buffer-disable-undo buf)))
    buf))

(defun mhc-kill-all-buffers ()
  "Kill all buffers for temporary use of MHC."
  (while mhc-tmp-buffer-list
    (if (buffer-name (car mhc-tmp-buffer-list))
        (kill-buffer (car mhc-tmp-buffer-list)))
    (setq mhc-tmp-buffer-list
          (cdr mhc-tmp-buffer-list))))


;;; Setup and exit

(defcustom mhc-setup-hook nil
  "Run hook after mhc-setup."
  :group 'mhc
  :type 'hook)

(defvar mhc-setup-p nil)

(defun mhc-setup ()
  (unless mhc-setup-p
    (condition-case nil
        (progn
          (or (featurep 'easymenu) (require 'easymenu))
          (easy-menu-define mhc-mode-menu
                            mhc-mode-map
                            "Menu used in mhc mode."
                            mhc-mode-menu-spec)
          (easy-menu-define mhc-calendar-mode-menu
                            mhc-calendar-mode-map
                            "Menu used in mhc calendar mode."
                            mhc-calendar-mode-menu-spec))
      (error nil))
    (or (assq 'mhc-mode minor-mode-alist)
        (setq minor-mode-alist
              (cons (list 'mhc-mode (mhc-file-line-status))
                    minor-mode-alist)))
    (or (assq 'mhc-mode minor-mode-map-alist)
        (setq minor-mode-map-alist
              (cons (cons 'mhc-mode mhc-mode-map)
                    minor-mode-map-alist)))
    (mhc-face-setup)
    (mhc-calendar-setup)
    (mhc-file-setup)
    (setq mhc-default-category-predicate-sexp
          (mhc-expr-compile mhc-default-category))
    (and (mhc-use-icon-p) (mhc-icon-setup))
    (and mhc-calendar-link-hnf (mhc-calendar-hnf-face-setup))
    (mhc-summary-line-inserter-setup)
    (mhc-guess-location-setup)
    (autoload 'mhc-ps "mhc-ps" "*Create PostScript calendar with selected method." t)
    (autoload 'mhc-ps-preview "mhc-ps" "*Preview PostScript calendar." t)
    (autoload 'mhc-ps-print "mhc-ps" "*Print PostScript calendar." t)
    (autoload 'mhc-ps-save "mhc-ps" "*Save PostScript calendar." t)
    (autoload 'mhc-ps-insert-buffer "mhc-ps" "*Insert PostScript calendar." t)
    (setq mhc-setup-p t)
    (run-hooks 'mhc-setup-hook)))

(defun mhc-reset ()
  "Reset MHC."
  (interactive)
  (message "MHC resetting...")
  (mhc-slot-clear-cache)
  (mhc-face-setup)
  (mhc-calendar-setup)
  (and (mhc-use-icon-p) (mhc-icon-setup))
  (and mhc-calendar-link-hnf (mhc-calendar-hnf-face-setup))
  (mhc-summary-line-inserter-setup)
  (mhc-guess-location-setup)
  (or (and (mhc-summary-buffer-p)
           (mhc-rescan-month mhc-default-hide-private-schedules))
      (and (mhc-calendar-p) (mhc-calendar-rescan)))
  (message "MHC resetting...done"))

(defcustom mhc-exit-hook nil
  "Run hook after mhc-exit."
  :group 'mhc
  :type 'hook)

(defun mhc-exit ()
  (setq mhc-setup-p nil)
  (mhc-file-exit)
  (mhc-slot-clear-cache)
  (mhc-kill-all-buffers)
  (run-hooks 'mhc-exit-hook))

(defun mhc-version ()
  "Show mhc version."
  (interactive)
  (message mhc-version))

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

;;; mhc.el ends here
