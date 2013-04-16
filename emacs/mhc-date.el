;;; mhc-date.el -- Digit style Date Calculation Lib.

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;;
;; Created: 2000/06/14
;; Revised: $Date: 2004/05/06 16:35:13 $

;;;
;;; Commentary:
;;;

;;
;; mhc-date format is simple. It expresses a date by
;; days from 1970/1/1
;;
;; for example:
;;
;; (mhc-date-new 1970 1  1) -> 0
;; (mhc-date-new 2000 6 14) -> 11122
;;
;; mhc-time is also simple. It expresses a time by minits from midnight.

;;;
;;; Code:
;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mhc-time

(defsubst mhc-time/check (HH MM)
  (and (integerp HH) (>= HH 0) (<= HH 99)
       (integerp MM) (>= MM 0) (<= MM 59)))

(defmacro mhc-time-HH (time)
  `(/ ,time 60))

(defmacro mhc-time-MM (time)
  `(% ,time 60))

;; All constructors emit error signal if args are illegal.
;; In case called with noerror is t, return nil quietly.

(defsubst mhc-time-new (HH MM &optional noerror)
  (if (mhc-time/check HH MM)
      (+ (* HH 60) MM)
    (if noerror
        nil
      (error "mhc-time-new: arg error (%s,%s)" HH MM))))

(defsubst mhc-time-new-from-string (str &optional noerror regexp)
  (let (ret (match (match-data)))
    (if (string-match (or regexp "^\\([0-9][0-9]\\):\\([0-9][0-9]\\)$") str)
        (setq ret (mhc-time-new (mhc-date/substring-to-int str 1)
                                (mhc-date/substring-to-int str 2)
                                t)))
    (store-match-data match)
    (if (or noerror ret)
        ret
      (error "mhc-time-new-from-string: format error (%s)" str))))

(defsubst mhc-time-now ()
  (let* ((now (decode-time (current-time)))
         (HH (nth 2 now))
         (MM (nth 1 now)))
    (mhc-time-new HH MM)))

;; xxx: use defmacro for speed !!
(defalias 'mhc-time-max 'max)
(defalias 'mhc-time-min 'min)
(defalias 'mhc-time<    '<)
(defalias 'mhc-time=    '=)
(defalias 'mhc-time<=   '<=)
(defalias 'mhc-time>    '>)
(defalias 'mhc-time>=   '>=)

(defun mhc-time-sort (time-list)
  (sort time-list (function mhc-time<)))

(defmacro mhc-time-let (time &rest form)
  (let ((tempvar (make-symbol "tempvar")))
    `(let* ((,tempvar ,time)
            (hh (mhc-time-HH ,tempvar))
            (mm (mhc-time-MM ,tempvar)))
       ,@form)))
(put 'mhc-time-let 'lisp-indent-function 1)
(put 'mhc-time-let 'edebug-form-spec '(form body))

(defmacro mhc-time-to-string (time)
  `(mhc-time-let ,time (format "%02d:%02d" hh mm)))

(defsubst mhc-time-to-list (time)
  (list (mhc-time-HH time)
        (mhc-time-MM time)))

(defalias 'mhc-time+  '+)
(defalias 'mhc-time-  '-)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; mhc-date

;;
;; special form.
;;

(defmacro mhc-date-let (date &rest form) "\
 This special form converts DATE, as the number of days since
 1970/01/01, to following local variables, and evaluates FORM.

      yy  The year, an integer typically greater than 1900.
      mm  The month of the year, as an integer between 1 and 12.
      dd  The day of the month, as an integer between 1 and 31.
      ww  The day of week, as an integer between 0 and 6, where 0
          stands for Sunday.
 "
  (let ((tempvar (make-symbol "tempvar")))
    `(let* ((,tempvar (mhc-date-to-list ,date))
            (yy (nth 0 ,tempvar))
            (mm (nth 1 ,tempvar))
            (dd (nth 2 ,tempvar))
            (ww (nth 3 ,tempvar)))
       ,@form)))
(put 'mhc-date-let 'lisp-indent-function 1)
(put 'mhc-date-let 'edebug-form-spec '(form body))


(defmacro mhc-date-let-for-month (date &rest form) "\
 This special form converts DATE, as the number of days since
 1970/01/01, to following local variables, and evaluates FORM.

      yy  The year, an integer typically greater than 1900.
      mm  The month of the year, as an integer between 1 and 12.
      dd  The day of the month, as an integer between 1 and 31.
      ww  The day of week, as an integer between 0 and 6, where 0
          stands for Sunday.
      oo  The order of week, as an integer between 0 and 4.
  last-p  Predicate to check if the dd is in the last week.
 "
  (let ((tempvar (make-symbol "tempvar")))
    `(let* ((,tempvar (mhc-date-to-list ,date))
            (yy (nth 0 ,tempvar))
            (mm (nth 1 ,tempvar))
            (dd 1)
            (ww (nth 3 ,tempvar))
            (end (mhc-date/last-day-of-month yy mm))
            (days ,date)
            (last-p nil))
       (while (<= dd end)
         ,@form
         (setq days   (mhc-date++ days)
               dd     (1+ dd)
               oo     (/ (1- dd) 7)
               ww     (% (1+ ww) 7)
               last-p (< (- end 7) dd))))))
(put 'mhc-date-let-for-month 'lisp-indent-function 1)
(put 'mhc-date-let-for-month 'edebug-form-spec '(form body))


;;
;; private
;;

(defsubst mhc-date/leap-year-p (yy)
  (and (zerop (% yy 4))
       (or (not (zerop (% yy 100)))
           (zerop (% yy 400)))))

(defsubst mhc-date/last-day-of-month (yy mm)
  (if (and (= mm 2) (mhc-date/leap-year-p yy))
      29
    (aref '[0 31 28 31 30 31 30 31 31 30 31 30 31] mm)))

(defsubst mhc-date/check (yy mm dd)
  (and (integerp yy) (>= yy 1000)
       (integerp mm) (>= mm 1) (<= mm 12)
       (integerp dd) (>= dd 1) (<= dd (mhc-date/last-day-of-month yy mm))
       t))

(defmacro mhc-date/day-number (yy mm dd)
  `(if (mhc-date/leap-year-p ,yy)
      (+ (aref '[0 0 31 60 91 121 152 182 213 244 274 305 335] ,mm) ,dd)
    (+ (aref '[0 0 31 59 90 120 151 181 212 243 273 304 334] ,mm) ,dd)))

(defsubst mhc-date/absolute-from-epoch (yy mm dd)
  (let ((xx (1- yy)))
    (+ (mhc-date/day-number yy mm dd)
       (* xx  365)
       (/ xx    4)
       (/ xx -100)
       (/ xx  400)
       -719163)))

(defsubst mhc-date/iso-week-days (yday wday)
  (- yday -3 (% (- yday wday -382) 7)))

(defmacro mhc-date/substring-to-int (str pos)
  `(string-to-int
    (substring ,str (match-beginning ,pos) (match-end ,pos))))

;; according to our current time zone,
;; convert timezone string into offset minutes
;;
;;   for example, if current time zone is in Japan,
;;   convert "GMT" or "+0000" into 540.
(defun mhc-date/string-to-timezone-offset (timezone)
  (let ((tz (or (cdr (assoc timezone
                            '(("PST" . "-0800") ("PDT" . "-0700")
                              ("MST" . "-0700") ("MDT" . "-0600")
                              ("CST" . "-0600") ("CDT" . "-0500")
                              ("EST" . "-0500") ("EDT" . "-0400")
                              ("AST" . "-0400") ("NST" . "-0300")
                              ("UT"  . "+0000") ("GMT" . "+0000")
                              ("BST" . "+0100") ("MET" . "+0100")
                              ("EET" . "+0200") ("JST" . "+0900"))))
                timezone))
        min
        offset)
    (if (string-match "\\([-+]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)" tz)
        (progn
          (setq min (* (+ (* 60 (mhc-date/substring-to-int tz 2))
                          (mhc-date/substring-to-int tz 3))
                       (if (string= "+"
                                    (substring tz
                                               (match-beginning 1)
                                               (match-end 1)))
                           1 -1))
                offset (- (/ (car (current-time-zone)) 60) min))))))

;;
;; conversion.
;;

(defsubst mhc-date-to-second (date)
  ;; It has workaround in case of 28 bit integer.
  (let (high low)
    (setq low  (* (+ date (if (< (nth 0 (current-time-zone)) 0) 1 0)) 240)
          high (/ low 65536)
          low  (* (% low 65536) 360)
          high (+ (* high 360) (/ low 65536))
          low  (% low 65536))
    (list high low 0)))


(defsubst mhc-date/to-list1 (date)
  (let ((lst (decode-time (mhc-date-to-second date))))
    (list (nth 5 lst)
          (nth 4 lst)
          (nth 3 lst)
          (nth 6 lst))))

(defsubst mhc-date/to-list2 (date)
  (let (x b c d e w dom)
    (setq w  (% (+ date 25568) 7)
          date (+ date 2440588)
          x (floor (/ (- date 1867216.25) 36524.25))
          b (- (+ date 1525 x) (floor (/ x 4.0)))
          c (floor (/ (- b 122.1) 365.25))
          d (floor (* 365.25 c))
          e (floor (/ (- b d) 30.6001))
          dom (- b d (floor (* 30.6001 e))))
    (if (<= e 13)
        (list (- c 4716) (1- e) dom w)
      (list (- c 4715) (- e 13) dom w))))

(defsubst mhc-date-to-list (date)
  (if (and (<= 0 date) (<= date 24837))
      (mhc-date/to-list1 date)
    (mhc-date/to-list2 date)))

;;
;; constructor.
;;

;; All constructors emit error signal if args are illegal.
;; In case called with noerror is t, return nil quietly.

;; new from 3 digits.
(defsubst mhc-date-new (yy mm dd &optional noerror)
  (if (mhc-date/check yy mm dd)
      (mhc-date/absolute-from-epoch yy mm dd)
    (if noerror
        nil
      (error "mhc-date-new: arg error (%s,%s,%s)" yy mm dd))))


;; new from emacs style time such as (14654 3252 689999).
(defsubst mhc-date-new-from-second (&optional second)
  (let ((now (decode-time (or second (current-time)))))
    (mhc-date/absolute-from-epoch
     (nth 5 now)
     (nth 4 now)
     (nth 3 now))))

;; new from current time.
(defalias 'mhc-date-now 'mhc-date-new-from-second)

;; new from string. 19990101
(defsubst mhc-date-new-from-string (str &optional noerror)
  (let (ret (match (match-data)))
    (if (string-match
         "^\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)$" str)
        (setq ret (mhc-date-new (mhc-date/substring-to-int str 1)
                                (mhc-date/substring-to-int str 2)
                                (mhc-date/substring-to-int str 3)
                                t)))
    (store-match-data match)
    (if (or noerror ret)
        ret
      (error "mhc-date-new-from-string: format error (%s)" str))))

;; new from string. [[yyyy/]mm]/dd
(defsubst mhc-date-new-from-string2 (str &optional base-date noerror)
  (mhc-date-let (or base-date (mhc-date-now))
    (let ((match (match-data)) fail ret)
      (cond
       ((string-match
         "^\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)$" str)
        (setq yy (mhc-date/substring-to-int str 1)
              mm (mhc-date/substring-to-int str 2)
              dd (mhc-date/substring-to-int str 3)))
       ((string-match "^\\([0-9]+\\)/\\([0-9]+\\)/\\([0-9]+\\)$" str)
        (setq yy (mhc-date/substring-to-int str 1)
              mm (mhc-date/substring-to-int str 2)
              dd (mhc-date/substring-to-int str 3)))
       ((string-match "^\\([0-9]+\\)/\\([0-9]+\\)$" str)
        (setq mm (mhc-date/substring-to-int str 1)
              dd (mhc-date/substring-to-int str 2)))
       ((string-match "^\\([0-9]+\\)$" str)
        (setq dd (mhc-date/substring-to-int str 1)))
       (t
        (setq fail t)))
      (store-match-data match)
      (if (not fail) (setq ret (mhc-date-new yy mm dd t)))
      (if (or noerror ret)
          ret
        (error "mhc-date-new-from-string2: format error (%s)" str)))))

;; regexp for rfc822 Date: field.
(defconst mhc-date/rfc822-date-regex
  ;; assuming  ``Tue,  9 May 2000 12:15:12 -0700 (PDT)''
  (concat
   "\\([0-9]+\\)[ \t]+"                                   ;; day
   "\\(Jan\\|Feb\\|Mar\\|Apr\\|May\\|Jun\\|"              ;;
   "Jul\\|Aug\\|Sep\\|Oct\\|Nov\\|Dec\\)[ \t]+"           ;; month
   "\\([0-9]+\\)[ \t]+"                                   ;; year
   "\\([0-9]+\\):\\([0-9]+\\)\\(:\\([0-9]+\\)\\)?[ \t]*"  ;; hh:mm(:ss)?
   "\\([A-Z][A-Z][A-Z]\\|[-+][0-9][0-9][0-9][0-9]\\)"     ;; JST or +0900
   ))

;; new from rfc822 Date: field.
(defun mhc-date-new-from-string3 (string)
  (if (and (stringp string) (string-match mhc-date/rfc822-date-regex string))
      (let ((dd  (mhc-date/substring-to-int string 1))
            (mm  nil)
            (mon (substring string (match-beginning 2) (match-end 2)))
            (yy  (mhc-date/substring-to-int string 3))
            (MM  (+ (* 60 (mhc-date/substring-to-int string 4))
                    (mhc-date/substring-to-int string 5)))
            (tz  (substring string (match-beginning 8) (match-end 8)))
            tz-offset)
        (setq
         yy (cond
             ((< yy 50)  (+ yy 2000))
             ((< yy 100) (+ yy 1900))
             (t            yy))
         mm (1+ (/ (string-match mon
                                 "JanFebMarAprMayJunJulAugSepOctNovDec") 3))
         tz-offset (mhc-date/string-to-timezone-offset tz)
         MM (+ MM tz-offset))
        (car
         (cond
          ((< MM 0)
           (setq MM (+ MM 1440))
           (list (mhc-date--  (mhc-date-new yy mm dd))
                 (mhc-time-new (/ MM 60) (% MM 60))
                 tz-offset))
          ((>= MM 1440)
           (setq MM (- MM 1440))
           (list (mhc-date++ (mhc-date-new yy mm dd))
                 (mhc-time-new (/ MM 60) (% MM 60))
                 tz-offset))
          (t
           (list (mhc-date-new yy mm dd)
                 (mhc-time-new (/ MM 60) (% MM 60))
                 tz-offset)))))))

;;
;; manipulate yy, mm, dd.
;;

(defmacro mhc-date-yy (date)
  `(nth 0 (mhc-date-to-list ,date)))

(defmacro mhc-date-mm (date)
  `(nth 1 (mhc-date-to-list ,date)))

(defmacro mhc-date-dd (date)
  `(nth 2 (mhc-date-to-list ,date)))

(defmacro mhc-date-ww (date)
  `(nth 3 (mhc-date-to-list ,date)))

(defmacro mhc-date-oo (date)
  `(/ (1- (mhc-date-dd ,date)) 7))

(defsubst mhc-date-cw (date)
  (mhc-date-let date
    (let* ((yday (mhc-date/day-number yy mm dd))
           (days (mhc-date/iso-week-days yday ww))
           (d))
      (if (< days 0)
          (setq days (mhc-date/iso-week-days
                      (+ yday 365 (if (mhc-date/leap-year-p (1- yy)) 1 0)) ww))
        (setq d (mhc-date/iso-week-days
                 (- yday 365 (if (mhc-date/leap-year-p yy) 1 0)) ww))
        (if (<= 0 d) (setq days d)))
      (1+ (/ days 7)))))

;;
;; compare.
;;

(defalias 'mhc-date=    '=  )
(defalias 'mhc-date<    '<  )
(defalias 'mhc-date<=   '<= )
(defalias 'mhc-date>    '>  )
(defalias 'mhc-date>=   '>= )

(defalias 'mhc-date-max 'max)
(defalias 'mhc-date-min 'min)
(defmacro mhc-date-sort    (date-list)
  `(sort ,date-list (function mhc-date<)))

(defsubst mhc-date-yy=  (d1 d2) (=  (mhc-date-yy d1) (mhc-date-yy d2)))
(defsubst mhc-date-yy<  (d1 d2) (<  (mhc-date-yy d1) (mhc-date-yy d2)))
(defsubst mhc-date-yy<= (d1 d2) (<= (mhc-date-yy d1) (mhc-date-yy d2)))
(defsubst mhc-date-yy>  (d1 d2) (mhc-date-yy<  d2 d1))
(defsubst mhc-date-yy>= (d1 d2) (mhc-date-yy<= d2 d1))

(defsubst mhc-date-yymm=  (d1 d2)
  (and (mhc-date-yy= d1 d2)
       (= (mhc-date-mm d1) (mhc-date-mm d2))))

(defsubst mhc-date-yymm<  (d1 d2)
  (or (mhc-date-yy< d1 d2)
      (and (mhc-date-yy= d1 d2)
           (< (mhc-date-mm d1) (mhc-date-mm d2)))))

(defmacro mhc-date-yymm>  (d1 d2)      `(mhc-date-yymm<  ,d2 ,d1))
(defmacro mhc-date-yymm<= (d1 d2) `(not (mhc-date-yymm>  ,d1 ,d2)))
(defmacro mhc-date-yymm>= (d1 d2)      `(mhc-date-yymm<= ,d2 ,d1))

;;
;; increment, decrement.
;;

(defalias 'mhc-date+  '+ )
(defalias 'mhc-date-  '- )
(defalias 'mhc-date++ '1+)
(defalias 'mhc-date-- '1-)

(defsubst mhc-date-mm+  (date c)
  (mhc-date-let date
    (let (xx pp)
      (setq xx (+ mm c))
      (setq pp (if (< 0 xx ) (/ (- xx  1) 12) (/ (- xx 12) 12)))
      (setq yy (+ yy pp) mm (- xx (* 12 pp)))
      (if (mhc-date/check yy mm dd)
          (mhc-date-new yy mm dd)
        (mhc-date-new yy mm (mhc-date/last-day-of-month yy mm))))))

(defmacro mhc-date-mm-  (date c) `(mhc-date-mm+ ,date (- ,c)))
(defmacro mhc-date-mm++ (date)   `(mhc-date-mm+ ,date 1))
(defmacro mhc-date-mm-- (date)   `(mhc-date-mm- ,date 1))

(defsubst mhc-date-yy+  (date c)
  (mhc-date-let date
    (setq yy (+ yy c))
    (if (mhc-date/check yy mm dd)
        (mhc-date-new yy mm dd)
      (mhc-date-new yy mm (mhc-date/last-day-of-month yy mm)))))

(defmacro mhc-date-yy-  (date c) `(mhc-date-yy+ ,date (- ,c)))
(defmacro mhc-date-yy++ (date)   `(mhc-date-yy+ ,date 1))
(defmacro mhc-date-yy-- (date)   `(mhc-date-yy- ,date 1))

;;
;; get meaninful date.
;;
(defmacro mhc-date-mm-first (date)
  "Return the number of days since 1970/01/01 to the first day of month, DATE."
  `(mhc-date-let ,date
     (mhc-date-new yy mm 1 t)))

(defmacro mhc-date-mm-last (date)
  "Return the number of days since 1970/01/01 to the last day of month, DATE."
  `(mhc-date-let ,date
     (mhc-date-new yy mm (mhc-date/last-day-of-month yy mm) t)))

;;
;; predicate
;;

;; check if the date is in the last week of a month.
(defsubst mhc-date-oo-last-p (date)
  (< (- (mhc-date/last-day-of-month
         (mhc-date-yy date)
         (mhc-date-mm date)) 7) (mhc-date-dd date)))


(defalias 'mhc-date-p 'integerp)


;;
;; miscellaneous.
;;

(defmacro mhc-end-day-of-week ()
  `(nth mhc-start-day-of-week '(6 0 1 2 3 4 5)))

;;
;; to string.
;;

;; (mhc-date-format date "%04d%02d%02d" yy mm dd)
(defmacro mhc-date-format (date format &rest vars)
  `(mhc-date-let ,date
     (format ,format ,@vars)))

(defun mhc-date-digit-to-mm-string (mm &optional long)
  (if long
      (aref
       '[nil "January" "February" "March"     "April"   "May"      "June"
             "July"    "August"   "September" "October" "November" "December"]
       mm)
    (aref
     [nil "Jan" "Feb" "Mar" "Apr" "May" "Jun"
          "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"]
     mm)))

(defun mhc-date-digit-to-ww-string (ww &optional long)
  (if long
      (aref ["Sunday" "Monday" "Tuesday" "Wednesday"
             "Thursday" "Friday" "Saturday"] ww)
    (aref ["Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"] ww)))

(defun mhc-date-digit-to-ww-japanese-string (ww &optional long)
  (if long
      (aref ["日曜日" "月曜日" "火曜日" "水曜日"
             "木曜日" "金曜日" "土曜日"] ww)
    (aref ["日" "月" "火" "水" "木" "金" "土"] ww)))

(defun mhc-date-digit-to-oo-string (oo &optional long)
  (aref  ["1st" "2nd" "3rd" "4th" "5th"] oo))

;; format-time-string subset (but has enough spec)
(defun mhc-date-format-time-string (format date)
  (mhc-date-let date
    (let (head match (ret "") char)
      (while (string-match "%." format)
        (setq head   (substring format 0 (match-beginning 0))
              match  (match-string 0 format)
              format (substring format (match-end 0))
              char   (aref match 1))
        (cond
         ((eq char ?Y) ;; 100年単位の年
          (setq match (format "%d" yy)))

         ((eq char ?y)  ;; 年の下2桁 (00-99)
          (setq match (format "%02d"  (% yy 100))))

         ((or (eq char ?b) (eq char ?h)) ;; 月   略称
          (setq match (mhc-date-digit-to-mm-string mm)))

         ((eq char ?B) ;; 月   名称
          (setq match (mhc-date-digit-to-mm-string mm t)))

         ((eq char ?m) ;; 月 (01-12)
          (setq match (format "%02d" mm)))

         ((eq char ?d) ;; 日 (ゼロ padding)
          (setq match (format "%02d" dd)))

         ((eq char ?e) ;; 日 (空白 padding)
          (setq match (format "%2d" dd)))

         ((eq char ?a) ;; 曜日 略称
          (setq match (mhc-date-digit-to-ww-string ww)))

         ((eq char ?A) ;; 曜日 名称
          (setq match (mhc-date-digit-to-ww-string ww t))))

        (setq ret (concat ret head match)))
      (concat ret format))))

(provide 'mhc-date)

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

;;; mhc-date.el ends here.
