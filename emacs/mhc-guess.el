;;; mhc-guess.el -- Guess the important date from a Japanese mail article.

;; Author:  Yoshinari Nomura <nom@quickhack.net>
;;
;; Created: 1999/04/13
;; Revised: $Date: 2007/12/05 04:59:35 $
;;

;;;
;;; Commentary:
;;;

;;
;; バッファから mhc-guess-{time,date}: 日付、時間を集めて、
;; 予定の日付けを表わしていると思われる可能性の高い順に並べて
;; 返す。
;;
;; 以下のような GUESS-CANDIDATE のリストを返す
;;   ([mhc-{date,time} mhc-{date,time}-end point-begin point-end score]..)
;;
;; mhc-{date,time}:     予定の開始 {日, 時間}
;; mhc-{date,time}-end  予定の終了 {日, 時間} or nil
;;
;; 日付推測の手順
;;
;; 1. 日付/時刻を表すキーワード見付けて、発見個所リストを作る。
;;
;;    (mhc-guess/gather-candidate mhc-guess-date-regexp-list now)
;;    (mhc-guess/gather-candidate mhc-guess-time-regexp-list now)
;;
;;    の 2つの関数で、
;;
;;    ([found-date found-date-end found-point-begin found-point-end nil] ...)
;;    ([found-time found-time-end found-point-begin found-point-end nil] ...)
;;
;;    のような candidate-list を得る。
;;
;; 2. みつかった日付時刻に点数をつける。
;;
;;    (mhc-guess/score candidate-list mhc-guess-keyword-score-alist)
;;
;;    ([found-date found-date-end found-point-begin found-point-end score] ...)
;;
;;      キーワードが引用行中にある
;;      同一行に特定の文字列がある
;;      ある範囲の前方/後方に特定の文字列がある
;;
;;    のような条件と加点/減点を表す mhc-guess-keyword-score-alist に基
;;    づいて採点をする。
;;
;; 3. 得点順 (得点が同じ場合は，日付や時間を表わす文字列が長い順)
;;    に sort して返す

;;;
;;; Code:
;;;

(require 'mhc-date)
(provide 'mhc-guess)

;;; Customize variables:

(defcustom mhc-guess-ignore-english-date nil
  "*Ignore English dates."
  :group 'mhc
  :type '(choice (const :tag "Ignore" t)
                 (const :tag "Don't Ignore" nil)))

(defcustom mhc-guess-english-date-format '(usa)
  "*English date formats.
You can specify following symbols as a list.
    usa: Suppose the USA style date formats. (e.g. Feb 25, 2004)
    british: Suppose British style date formats. (e.g. 25 Feb, 2004)"
  :group 'mhc
  :type '(repeat (choice (const :tag "USA" usa)
                         (const :tag "British" british))))

;;
;; regexp for get date strings.
;;

(defvar mhc-guess-date-regexp-list
  `(
    (,(concat "\\([０-９0-9][０-９0-9][０-９0-9][０-９0-9]\\)[-−/／]"
              "\\([０-９0-9][０-９0-9]\\)[-−/／]"
              "\\([０-９0-9][０-９0-9]\\)")
     mhc-guess/make-date-from-yyyymmdd 1 2 3)

    (,(concat "\\([０-９0-9]+年\\)?"
              "\\([来今０-９0-9]+\\)[\n　 ]*月[\n 　]*の?[\n 　]*"
              "\\([０-９0-9]+\\)日?"
              "\\([（）()月火水木金土日曜\n 　 ]*"
              "\\([〜−-，,、]\\|から\\|より\\)[\n 　]*"
              "\\([０-９0-9]+年\\)?"
              "\\(\\([来今０-９0-9]+\\)[\n　 ]*月\\)?[\n 　]*の?[\n 　]*"
              "\\([０-９0-9]+\\)日?\\(間\\)?"
              "\\)?")
     mhc-guess/make-date-from-mmdd 2 3 8 9 10)

    (,(concat "\\([０-９0-9]+[　 ]*[／/][　 ]*\\)?"
              "\\([０-９0-9]+\\)[　 ]*[／/][　 ]*\\([０-９0-9]+\\)"
              "\\([（）()月火水木金土日曜\n 　 ]*"
                   "\\([〜−，,、-]\\|から\\|より\\)[\n 　]*"
                   "\\([０-９0-9]+[　 ]*[／/][　 ]*\\)?"
                   "\\(\\([０-９0-9]+\\)[　 ]*[／/][　 ]*\\)"
                   "\\([０-９0-9]+\\)日?\\(間\\)?"
              "\\)?")
     mhc-guess/make-date-from-mmdd 2 3 8 9 10)

    ;; USA style date format
    (,(concat "\\(Jan\\(uary\\)?\\|Feb\\(ruary\\)?\\|Mar\\(ch\\)?\\|"
              "Apr\\(il\\)?\\|May\\|June?\\|July?\\|Aug\\(ust\\)?\\|"
              "Sep\\(tember\\)?\\|Oct\\(ober\\)?\\|"
              "Nov\\(ember\\)?\\|Dec\\(ember\\)?\\)"
              "\.?,? +"
              "\\([0-9][0-9]?\\)\\(st\\|nd\\rd\\|th\\)?,?[ \n]+" ;; day
              "\\(\\('\\|[1-9][0-9]\\)?[0-9][0-9]\\)?") ;; year
     mhc-guess/make-date-from-usa-style-date 1 11 13)

    ;; British style date format
    (,(concat "\\([0-9][0-9]?\\)\\(st\\|nd\\rd\\|th\\)?,? " ;; day
              "\\(Jan\\(uary\\)?\\|Feb\\(ruary\\)?\\|Mar\\(ch\\)?\\|"
              "Apr\\(il\\)?\\|May\\|June?\\|July?\\|Aug\\(ust\\)?\\|"
              "Sep\\(tember\\)?\\|Oct\\(ober\\)?\\|"
              "Nov\\(ember\\)?\\|Dec\\(ember\\)?\\)"
              "\.?,?[ \n]+"
              "\\(\\('\\|[1-9][0-9]\\)?[0-9][0-9]\\)?") ;; year
     mhc-guess/make-date-from-british-style-date 1 3 13)

    throw

    (,(concat "\\(今度\\|[今来次]週\\|再来週\\)[\n 　]*の?[\n 　]*"
             "\\([月火水木金土日]\\)曜")
     mhc-guess/make-date-from-relative-week 1 2)

    (,(concat "\\([Tt]his\\|[Nn]ext\\)[\n ]+"
              "\\(Monday\\|Tuesday\\|Wednesday\\|Thursday\\|Friday\\|"
              "Saturday\\|Sunday\\)")
     mhc-guess/make-date-from-english-relative-week 2 1 nil)

    (,(concat "\\(Monday\\|Tuesday\\|Wednesday\\|Thursday\\|Friday\\|"
              "Saturday\\|Sunday\\)[\n ]+"
              "\\([Tt]his\\|[Nn]ext\\)[ \n]+\\([Ww]eek\\)")
     mhc-guess/make-date-from-english-relative-week 1 2 3)

    throw

    ("\\([０-９0-9]+\\)[\n 　]*日"
     mhc-guess/make-date-from-mmdd nil 1)

    ("\\([０-９0-9]+\\)[ 　]*[(（][月火水木金土日]"
     mhc-guess/make-date-from-mmdd nil 1)

    ("[^\(（]\\([月火水木金土日]\\)\n?曜"
     mhc-guess/make-date-from-relative-week nil 1)

    (,(concat "\\(Monday\\|Tuesday\\|Wednesday\\|Thursday\\|Friday\\|"
              "Saturday\\|Sunday\\)")
     mhc-guess/make-date-from-english-relative-week 1 nil nil)

    ("\\(本日\\|今日\\|あす\\|あした\\|あさって\\|明日\\|明後日\\)"
     mhc-guess/make-date-from-relative-day 1)

    (,(concat "\\([Tt]oday\\|[Tt]omorrow\\|"
              "[Tt]he[ \n]+[Dd]ay[ \n]+[Aa]fter[ \n]+[Tt]omorrow\\)")
     mhc-guess/make-date-from-english-relative-day 1)
    ))

(defvar mhc-guess-time-regexp-list
  `(
    (,(concat "\\([０-９0-9]+\\) *[時] *\\([０-９0-9]+\\|半\\)?分?"
              "\\([\n 　]*\\([〜−-]\\|から\\|より\\)[\n 　午前後]*"
              "\\([０-９0-9]+\\) *[時]\\(間\\)? *\\([０-９0-9]+\\|半\\)?\\)?")
     mhc-guess/make-time-from-hhmm 1 2 5 7 6)
    (,(concat "\\([０-９0-9]+\\)[：:]\\([０-９0-9]+\\)"
              "\\([\n 　]*\\([〜−-]\\|から\\|より\\)[\n 　午前後]*"
              "\\([０-９0-9]+\\) *[：:時]\\(間\\)? *\\([０-９0-9]+\\|半\\)?\\)?")
     mhc-guess/make-time-from-hhmm 1 2 5 7 6)
    ))

(defvar mhc-guess-location-list '()
  "*List of the regexps of the location, like this
  '(\"第?[0-9０-９〇-九]+応接室?\"
    \"第?[0-9０-９〇-九]+会議室[0-9０-９〇-九]?\"))")

(defvar mhc-guess-location-regexp-list
  `(
    (,(concat "場[ 　]*所[ 　]*[：:]*[\n 　]*\\([^\n 　]+\\)")
     mhc-guess/make-location-from-string 1)
    (,(concat "於[ 　]*\\([^\n 　]+\\)")
     mhc-guess/make-location-from-string 1)
    (,(concat "[@＠][　 ]*\\([^\n　 .]+\\)[　 \n]")
     mhc-guess/make-location-from-string 1)))

;; keyword to score-alist:
;;    each element consists of (regexp relative-boundary sameline? score)
(defvar mhc-guess-keyword-score-alist
  '(
    ;; positive factor
    ("^[\t ]+"                                      -200 t   +5)
    ("次回"                                         -200 nil +10)
    ("\\(期間\\|月日\\|日程\\|時間帯\\|日時\\|開始時間\\)"        -150 nil +5)
    ("\\(期間\\|月日\\|日程\\|時間帯\\|日時\\|開始時間\\)[:：]"   -150 t   +5)
    ("\\(から\\|〜\\|変更\\|延期\\|順延\\|開始\\)"   +80 nil +4)
    ;; negative factor
    ("\\(休み\\|除く\\|中止\\|までに\\)"             +80 t   -10)
    ("出欠"                                          -80 nil -5)
    ("^\\(On\\|At\\|Date:\\) "                      -200 t   -20)
    ("\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\)"      -200 t   -20)
    ("\\(Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)"      -200 t   -20)
    ("^\\([ a-zA-Z]*>\\)+ *"                        -200 t   -15)
    ))

(defvar mhc-guess/location-regexp-list nil)

;;
;; manipulate guess-candidate structure.
;;

(defmacro mhc-guess-get-date             (obj)   `(aref ,obj 0))
(defmacro mhc-guess-get-time             (obj)   `(aref ,obj 0))
(defmacro mhc-guess-get-date-or-time     (obj)   `(aref ,obj 0))

(defmacro mhc-guess-get-date-end         (obj)   `(aref ,obj 1))
(defmacro mhc-guess-get-time-end         (obj)   `(aref ,obj 1))
(defmacro mhc-guess-get-date-or-time-end (obj)   `(aref ,obj 1))

(defmacro mhc-guess-get-begin         (obj)   `(aref ,obj 2))
(defmacro mhc-guess-get-end           (obj)   `(aref ,obj 3))
(defmacro mhc-guess-get-score         (obj)   `(aref ,obj 4))

(defmacro mhc-guess-set-date      (obj val) `(aset ,obj 0 ,val))
(defmacro mhc-guess-set-time      (obj val) `(aset ,obj 0 ,val))

(defmacro mhc-guess-set-date-end  (obj val) `(aset ,obj 1 ,val))
(defmacro mhc-guess-set-time-end  (obj val) `(aset ,obj 1 ,val))

(defmacro mhc-guess-set-begin     (obj val) `(aset ,obj 2 ,val))
(defmacro mhc-guess-set-end       (obj val) `(aset ,obj 3 ,val))
(defmacro mhc-guess-set-score     (obj val) `(aset ,obj 4 ,val))

(defun mhc-guess/new (&optional date-or-time date-or-time-end begin end score)
  (vector date-or-time date-or-time-end begin end score))

;;
;; pulic entry
;;

(defun mhc-guess-date (&optional hint1)
  (let ((now (or (mhc-date-new-from-string3 (mhc-header-get-value "Date"))
                 (mhc-date-now))))
    (mhc-guess/guess mhc-guess-date-regexp-list hint1 now)))

(defun mhc-guess-time (&optional hint1)
  (mhc-guess/guess mhc-guess-time-regexp-list hint1))

(defun mhc-guess-location-setup ()
  (if mhc-guess-location-list
      (let ((list mhc-guess-location-list)
            regex)
        (while list
          (setq regex (concat regex "\\(" (car list) "\\)"))
          (setq list (cdr list))
          (when list (setq regex (concat regex "\\|"))))
        (setq mhc-guess/location-regexp-list
              (cons `(,regex mhc-guess/make-location-from-string 0)
                    mhc-guess-location-regexp-list)))
    (setq mhc-guess/location-regexp-list mhc-guess-location-regexp-list)))

(defun mhc-guess-location (&optional hint1)
  (mhc-guess/guess mhc-guess/location-regexp-list hint1))

(defun mhc-guess/guess (control-regexp-lst &optional hint1 now)
  (let ((score-list
         (mhc-guess/score (mhc-guess/gather-candidate control-regexp-lst now)
                          mhc-guess-keyword-score-alist
                          hint1
                          now)))
    (sort score-list
          (function (lambda (a b)
                      (if (= (mhc-guess-get-score a) (mhc-guess-get-score b))
                          (< (- (mhc-guess-get-end b) (mhc-guess-get-begin b))
                             (- (mhc-guess-get-end a) (mhc-guess-get-begin a)))
                        (< (mhc-guess-get-score b)
                           (mhc-guess-get-score a))))))))
;;
;; gather date/time.
;;

(defun mhc-guess/gather-candidate (control-regexp-lst &optional now)
  (let ((ret nil) cand-lst)
    (while control-regexp-lst
      (cond
       ((listp (car control-regexp-lst))
        (if (setq cand-lst
                  (mhc-guess/gather-candidate2
                   (car (car control-regexp-lst))       ;; regexp
                   (car (cdr (car control-regexp-lst))) ;; convfunc
                   (cdr (cdr (car control-regexp-lst))) ;; posision list
                   now                          ;; current date
                   ))
            (setq ret (nconc ret cand-lst))))
       ((and (string= "throw" (symbol-name (car control-regexp-lst))) ret)
        (setq control-regexp-lst nil)))
      (setq control-regexp-lst (cdr control-regexp-lst)))
    ret))

(defun mhc-guess/gather-candidate2 (regexp convfunc pos-list &optional now)
  (let* (lst duration param-list p)
    (save-excursion
      ;; skip Header
      (goto-char (point-min))
      (re-search-forward "^-*$" nil t)
      ;; search candities.
      (while (re-search-forward regexp nil t)
        (setq  p pos-list
               param-list nil)
        (while p
          (setq param-list
                (cons
                 (if (and (car p) (match-beginning (car p)))
                     (buffer-substring (match-beginning (car p))
                                       (match-end       (car p)))
                   nil)
                 param-list))
          (setq p (cdr p)))
        (setq duration (apply 'funcall convfunc now (nreverse param-list)))
        (if (car duration)
            (setq lst
                  (cons
                   (mhc-guess/new (car duration)
                                  (cdr duration)
                                  (match-beginning 0)
                                  (match-end 0)
                                  nil)
                   lst)))))
    (nreverse lst)))

;;
;; make date from string.
;;


(defun mhc-guess/make-date-from-yyyymmdd (now yy-str mm-str dd-str)
  (let (date)
    (if (setq date (mhc-date-new
                    (mhc-guess/string-to-int yy-str)
                    (mhc-guess/string-to-int mm-str)
                    (mhc-guess/string-to-int dd-str)
                    t)) ; noerror is t.
        (cons date nil))))

(defun mhc-guess/make-date-from-mmdd
  (now mm-str dd-str &optional mm-str2 dd-str2 relative)
  (let* ((start nil) (end nil))
    (setq start (mhc-guess/make-date-from-mmdd2 now mm-str dd-str))
    (if start
        (setq end (mhc-guess/make-date-from-mmdd2 start mm-str2 dd-str2)))
    (cond
     ((null start)
      nil)
     ((null end)
      (cons start nil))
     (relative
      (cons start (mhc-date+ start end)))
     (t
      (cons start end)))))

(defun mhc-guess/make-date-from-mmdd2 (now mm-str dd-str)
  (let ((data (match-data))
        (mm (if mm-str (mhc-guess/string-to-int mm-str) 0))
        (dd (if dd-str (mhc-guess/string-to-int dd-str) 0))
        (year-offset 0)
        date)
    (cond
     ((string= mm-str "来")
      (setq mm (mhc-date-mm (mhc-date-mm++ now))))
     ((string= mm-str "今")
      (setq mm (mhc-date-mm now)))
     ((= mm 0)
      (setq mm (mhc-date-mm now))))
    (if (not
         (setq date
               (mhc-date-new (mhc-date-yy now) mm dd t))) ;; noerror is t
        ()
      ;; if date is past, assume the next year.
      (if (mhc-date< date now)
          (setq year-offset (1+ year-offset)))
      ;; if date is far future, assume the last year.
      (if (< 300 (+ (mhc-date- date now) (* year-offset 365)))
          (setq year-offset (1- year-offset)))
      (setq date (mhc-date-yy+ date year-offset)))
    (store-match-data data)
    date))

(defun mhc-guess/make-date-from-usa-style-date (now month-str dd-str yy-str)
  (if (and (null mhc-guess-ignore-english-date)
           (memq 'usa mhc-guess-english-date-format))
      (mhc-guess/make-date-from-english-date now month-str dd-str yy-str)))

(defun mhc-guess/make-date-from-british-style-date (now dd-str month-str yy-str)
  (if (and (null mhc-guess-ignore-english-date)
           (memq 'british mhc-guess-english-date-format))
      (mhc-guess/make-date-from-english-date now month-str dd-str yy-str)))

(defun mhc-guess/make-date-from-english-date (now month-str dd-str yy-str)
  (let* ((month-alist
          '(("Jan" . "1") ("Feb" . "2") ("Mar" . "3") ("Apr" . "4")
            ("May" . "5") ("Jun" . "6") ("Jul" . "7") ("Aug" . "8")
            ("Sep" . "9") ("Oct" . "10") ("Nov" . "11") ("Dec" . "12")))
         (mm-str (cdr (assoc (capitalize (substring month-str 0 3))
                             month-alist)))
         (yy-length (length yy-str)))
    (cond ((= yy-length 4)              ; "yyyy"
           (mhc-guess/make-date-from-yyyymmdd now yy-str mm-str dd-str))
          ((or (= yy-length 3) (= yy-length 2)) ; "'yy" or "yy"
           (mhc-guess/make-date-from-yyyymmdd
            now
            (concat (substring (format-time-string "%Y") 0 2)
                    (substring yy-str -2))
            mm-str dd-str))
          (t
           (mhc-guess/make-date-from-mmdd now mm-str dd-str)))))

(defun mhc-guess/make-date-from-relative-day (now rel-word)
  (cond
   ((null rel-word)
    nil)
   ((or (string= rel-word "今日") (string= rel-word "本日"))
    (cons now nil))
   ((or (string= rel-word "あす")
        (string= rel-word "あした")
        (string= rel-word "明日"))
    (cons (mhc-date++ now) nil))
   ((or (string= rel-word "あさって")
        (string= rel-word "明後日"))
    (cons (mhc-date+ now 2) nil))))

(defun mhc-guess/make-date-from-english-relative-day (now rel-word)
  (unless mhc-guess-ignore-english-date
    (let ((rel (downcase rel-word)))
      (cond
       ((null rel)
        nil)
       ((string= rel "today")
        (cons now nil))
       ((string= rel "tomorrow")
        (cons (mhc-date++ now) nil))
       (t ;; the day after tommorow.
        (cons (mhc-date+ now 2) nil))))))

(defun mhc-guess/make-date-from-relative-week (now rel-word week)
  (let ((data (match-data))
        (ww (string-match week "日月火水木金土"))
        (date (or now (mhc-date-now)))
        off)
    (setq off  (- ww (mhc-date-ww date)))
    (if (string= week "日") (setq off (+ 7 off)))
    (setq off
          (cond
           ((or (null rel-word)
                (string= rel-word "今度")
                (string= rel-word "次"))
            (if (<= off 0) (+ 7 off) off))
           ((string= rel-word "今週") off)
           ((string= rel-word "来週")
            (+ off 7))
           ((string= rel-word "再来週")
            (+ off 14))))
    (store-match-data data)
    (cons (mhc-date+ date off) nil)
    ))

(defun mhc-guess/make-date-from-english-relative-week (now dow rel-word week)
  (unless mhc-guess-ignore-english-date
    (let ((dow-alist '(("Monday" . "月") ("Tuesday" . "火")
                       ("Wednesday" . "水") ("Thursday" . "木")
                       ("Friday" . "金") ("Saturday" . "土")
                       ("Sunday" . "日")))
          (rel (when (stringp rel-word)
                 (downcase rel-word))))
      (mhc-guess/make-date-from-relative-week
       now
       (if (null rel)
           nil
         (cond ((and (string= rel "this") (null week))
                "今度")
               ((and (string= rel "this") week)
                "今週")
               ((and (string= rel "next") (null week))
                "今度")
               ((and (string= rel "next") week)
                "来週")
               (t
                nil)))
       (cdr (assoc-ignore-case dow dow-alist))))))

;;
;; make time from string.
;;

(defun mhc-guess/make-time-from-hhmm
  (now hh-str mm-str hh-str2 mm-str2 &optional relative)
  (let ((start (mhc-guess/make-time-from-hhmm2 hh-str mm-str))
        (end   (mhc-guess/make-time-from-hhmm2 hh-str2 mm-str2 relative)))
    (cond
     ((null start)
      nil)
     ((null end)
      (cons start nil))
     (relative
      (cons start (mhc-time+ start end)))
     (t
      (cons start end)))))

(defun mhc-guess/make-time-from-hhmm2 (hh-str mm-str &optional relative)
  (let (xHH xMM)
    (if (null hh-str)
        nil  ;; retun value

      (setq xHH (mhc-guess/string-to-int hh-str))
      (if (and (not relative) (< xHH 8)) ;; 8 depends on my life style.
          (setq xHH (+ xHH 12)))
      (setq xMM
            (cond
             ((not mm-str)           0)
           ((string= mm-str "半")    30)
           (t                        (mhc-guess/string-to-int mm-str))))
      (mhc-time-new xHH xMM t))))

;;
;; make location from string
;;

(defun mhc-guess/make-location-from-string (now str)
  (cons str nil))

;;
;; scoring
;;

(defun mhc-guess/score (candidate-lst score-alist &optional hint1 now)
  (let ((clist candidate-lst)
        total-score candidate regexp boundary sameline score slist)
    (while clist
      (setq candidate   (car clist)
            slist       score-alist
            total-score 0)

      ;; set score using score-alist
      (while slist
        (setq regexp   (nth 0 (car slist))
              boundary (nth 1 (car slist))
              sameline (nth 2 (car slist))
              score    (nth 3 (car slist)))
        (if (mhc-guess/search-in-boundary
             regexp
             (mhc-guess-get-begin candidate)
             boundary
             sameline)
            (setq total-score (+ total-score score)))
        (setq slist (cdr slist)))

      ;; hint1 is a position hint to encourage the near one.
      (if (and hint1
               (<  hint1 (mhc-guess-get-begin candidate))
               (<  (- (mhc-guess-get-begin candidate) hint1) 100))
          (setq total-score (+ total-score 10)))

      ;; now is a date hint to discourage a past date.
      (if (and now (mhc-date<= (mhc-guess-get-date candidate) now))
          (setq total-score (- total-score 5)))

      (mhc-guess-set-score candidate total-score)
      (setq clist (cdr clist)))
    candidate-lst))

(defun mhc-guess/search-in-boundary (regexp ptr rel-boundary sameline)
  (let ((pmin (+ ptr rel-boundary)) (pmax (+ ptr rel-boundary)))
    (save-excursion
      (goto-char ptr)
      (if sameline
          (setq pmax (min pmax (save-excursion (end-of-line)       (point)))
                pmin (max pmin (save-excursion (beginning-of-line) (point)))))
      (if (< 0 rel-boundary)
          (and (< (point) pmax) (search-forward-regexp regexp pmax t))
        (and (< pmin (point)) (search-backward-regexp regexp pmin t))))))

;;
;; string-to-int with code conversion.
;;

(defconst mhc-guess/zenkaku-hankaku-alist
  '(("０" . "0") ("１" . "1") ("２" . "2") ("３" . "3") ("４" . "4")
    ("５" . "5") ("６" . "6") ("７" . "7") ("８" . "8") ("９" . "9")
    ("／" . "/") ("：" . ":")))

(defun mhc-guess/string-to-int (str)
  (let ((chr "") (ret "") (data (match-data))
        (z2h-alist
         '(("０" . "0") ("１" . "1") ("２" . "2") ("３" . "3") ("４" . "4")
           ("５" . "5") ("６" . "6") ("７" . "7") ("８" . "8") ("９" . "9")
           ("／" . "/") ("：" . ":"))))
    (while (string-match "^." str)
      (setq chr (substring str (match-beginning 0) (match-end 0)))
      (setq ret (concat ret (or (cdr (assoc chr z2h-alist)) chr)))
      (setq str (substring str (match-end 0))))
    (store-match-data data)
    (string-to-int ret)))


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

;;; mhc-guess.el ends here


