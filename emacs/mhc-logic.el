;;; -*- mode: Emacs-Lisp; coding: utf-8 -*-

;; Author:  TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Created: 2000/04/30
;; Revised: $Date$


;;; Commentary:

;; This file is a part of MHC.

;; スケジュールの条件を表すヘッダを、その条件と等しいS式に変換するため
;; のライブラリ。

;; S式は、以下のようなローカル変数の束縛の下で評価される。

;;     (let ((month 4)
;;           (day 11048) ; 1970/1/1 からの日数
;;           (day-of-month 1)
;;           (day-of-week 6) ; 0 = Sun, 1 = Mon, ...
;;           (week-of-month 0) ; 0 = 1st, 1 = 2nd, 2 = 3rd, 3 = 4th, 4 = 5th
;;           (last-week nil)
;;           (todo nil))
;;       (eval sexp))

;; 具体的な評価の形式は、mhc-logic-eval-for-date, mhc-db/eval-for-duration
;; 関数の定義などを参照。

;; 条件が、Emacs-Lisp の述語のみからなるS式に変換されると、元々の条件
;; の意味が分かりづらくなるため、一旦、元々のヘッダとほとんど同じ形式
;; のマクロを用いた式に変換する。

;; この中間式を参照することによって、元々の条件に対する意味論的な評価
;; が可能となる(mhc-logic-file-to-slot)。

;; また、通常の評価を行う場合は、中間式に含まれるマクロを完全に展開し
;; てから行うため(mhc-logic-compile-file)、スピードは高速に保たれる。

;;; Definition:
(require 'mhc-date)
(require 'bytecomp)

;;----------------------------------------------------------------------
;;              MHC-LOGIC 構造体
;;----------------------------------------------------------------------

;; MHC-LOGIC    ::= [ DAY AND TODO INTERMEDIATE SEXP ]
;; DAY          ::= INT | NOT_INT
;; NOT_INT      ::= ( INT . nil )
;; INT          ::= integer ( represents exceptional date )
;; AND          ::= conditions ( each condition represents X-SC-Cond: header )
;; INTERMEDIATE ::= macro expression
;; SEXP         ::= full expanded expression

;; mhc-logic/day          = 日付(X-SC-Day)による条件
;; mhc-logic/and          = それ以外のヘッダに基づく条件
;; mhc-logic/todo         = TODOの順位
;; mhc-logic/intermediate = 条件をS式に変換するための中間形式
;; mhc-logic-sexp         = 完全に展開されたS式

(defun mhc-logic-new ()
  (make-vector 5 nil))

(defmacro mhc-logic/day (logicinfo)
  `(aref ,logicinfo 0))
(defmacro mhc-logic/and (logicinfo)
  `(aref ,logicinfo 1))
(defmacro mhc-logic-todo (logicinfo)
  `(aref ,logicinfo 2))
(defmacro mhc-logic/intermediate (logicinfo)
  `(aref ,logicinfo 3))
(defmacro mhc-logic-sexp (logicinfo)
  `(aref ,logicinfo 4))

(defmacro mhc-logic/set-day (logicinfo value)
  `(aset ,logicinfo 0 ,value))
(defmacro mhc-logic/set-and (logicinfo value)
  `(aset ,logicinfo 1 ,value))
(defmacro mhc-logic/set-todo (logicinfo value)
  `(aset ,logicinfo 2 ,value))
(defmacro mhc-logic/set-intermediate (logicinfo value)
  `(aset ,logicinfo 3 ,value))
(defmacro mhc-logic/set-sexp (logicinfo value)
  `(aset ,logicinfo 4 ,value))

(defun mhc-logic-day-as-string-list (logicinfo)
  (mapcar (lambda (day)
            (if (consp day)
                (mhc-date-format (car day) "!%04d%02d%02d" yy mm dd)
              (mhc-date-format day "%04d%02d%02d" yy mm dd)))
          (mhc-logic/day logicinfo)))



;;----------------------------------------------------------------------
;;              条件式を評価する関数
;;----------------------------------------------------------------------

(defun mhc-logic-eval-for-date (sexp-list day &optional todo)
  "指定された日のスケジュールを探索"
  (mhc-day-let day
    (let ((week-of-month (/ (+ day-of-month
                               (mhc-date-ww (mhc-date-mm-first day))
                               -8)
                            7))
          (last-week (> 7 (- (mhc-date/last-day-of-month year month)
                             day-of-month)))
          (new (mhc-day-new year month day-of-month day-of-week)))
      (mhc-day-set-schedules new (delq nil
                                       (mapcar
                                        (lambda (sexp)
                                          (and sexp
                                               (funcall sexp))) sexp-list)))
      new)))



;;----------------------------------------------------------------------
;;              条件式を生成するための関数群
;;----------------------------------------------------------------------

;; S式を表現する中間形式のマクロ
;; これらは、条件式の意味論的表示として用いられる。
(defmacro mhc-logic/condition-month (n) `(eq month ,n))
(defmacro mhc-logic/condition-day (n) `(eq day ,n))
(defmacro mhc-logic/condition-day-of-month (n) `(eq day-of-month ,n))
(defmacro mhc-logic/condition-day-of-week (n) `(eq day-of-week ,n))
(defmacro mhc-logic/condition-week-of-month (n) `(eq week-of-month ,n))
(defmacro mhc-logic/condition-last-week () 'last-week)
(defmacro mhc-logic/condition-duration (begin end) `(and (>= day ,begin) (<= day ,end)))
(defmacro mhc-logic/condition-duration-begin (begin) `(>= day ,begin))
(defmacro mhc-logic/condition-duration-end (end) `(<= day ,end))

(defconst mhc-logic/space-regexp "[,| \t\n]+"
  "構文要素の区切りに一致する正規表現")

(defconst mhc-logic/not-regexp "\\(!\\)?[ \t]*"
  "構文要素の否定に一致する正規表現")

(defconst mhc-logic/day-regexp
  "\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)\\([0-9][0-9]\\)"
  "構文要素の日付に一致する正規表現")

(defconst mhc-logic/day-of-month-regexp
  "0*\\([1-9]\\|[1-2][0-9]\\|3[01]\\)"
  "構文要素の該当月の何日目かを表す序数に一致する正規表現")

(defconst mhc-logic/week-of-month-alist
  '(("1st" 0 (mhc-logic/condition-week-of-month 0))
    ("2nd" 1 (mhc-logic/condition-week-of-month 1))
    ("3rd" 2 (mhc-logic/condition-week-of-month 2))
    ("4th" 3 (mhc-logic/condition-week-of-month 3))
    ("5th" 4 (mhc-logic/condition-week-of-month 4))
    ("last" 5 (mhc-logic/condition-last-week)))
  "構文要素の該当月の何週目かを表す序数の連想配列")

(defconst mhc-logic/week-of-month-regexp
  (mhc-regexp-opt (mapcar (function car) mhc-logic/week-of-month-alist) 'paren)
  "構文要素の何週目かを表す序数に一致する正規表現")

(defconst mhc-logic/day-of-week-alist
  '(("sun" . 0) ("mon" . 1) ("tue" . 2) ("wed" . 3) ("thu" . 4) ("fri" . 5) ("sat" . 6)
    ("sunday" . 0) ("monday" . 1) ("tuesday" . 2) ("wednesday" . 3) ("thursday" . 4)
    ("friday" . 5) ("saturday" . 6))
  "構文要素の曜日の連想配列")

(defconst mhc-logic/day-of-week-regexp
  (mhc-regexp-opt (mapcar (function car) mhc-logic/day-of-week-alist) 'paren)
  "構文要素の曜日に一致する正規表現")

(defconst mhc-logic/month-alist
  '(("jan" . 1) ("feb" . 2) ("mar" . 3) ("apr" . 4) ("may" . 5) ("jun" . 6)
    ("jul" . 7) ("aug" . 8) ("sep" . 9) ("oct" . 10) ("nov" . 11) ("dec" . 12)
    ("january" . 1) ("february" . 2) ("march" . 3) ("april" . 4) ("june" . 6)
    ("july" . 7) ("august" . 8) ("september" . 9) ("october" .10) ("november" . 11)
    ("december" . 12))
  "構文要素の月の連想配列")

(defconst mhc-logic/month-regexp
  (mhc-regexp-opt (mapcar (function car) mhc-logic/month-alist) 'paren)
  "構文要素の月に一致する正規表現")

(defconst mhc-logic/old-style-date-regexp
  "\\([0-9]+\\)[\t ]+\\([A-Z][a-z][a-z]\\)[\t ]+\\([0-9]+\\)"
  "構文要素の旧形式の日付指定に一致する正規表現")


(defmacro mhc-logic/looking-at (&rest regexp)
  "正規表現に一致する構文要素を発見するマクロ"
  `(looking-at (concat ,@regexp mhc-logic/space-regexp)))


(defun mhc-logic-parse-day (logicinfo)
  "X-SC-Day: ヘッダを解析する関数"
  (let ((d) (days (mhc-logic/day logicinfo)))
    (if (looking-at mhc-logic/space-regexp)
        (goto-char (match-end 0)))
    (while (not (eobp))
      (or (mhc-logic/looking-at mhc-logic/not-regexp mhc-logic/day-regexp)
          (error "Parse ERROR !!! (at X-SC-Day:)"))
      (setq d (mhc-date-new (string-to-number (match-string 2))
                            (string-to-number (match-string 3))
                            (string-to-number (match-string 4)))
            days (cons (if (match-string 1) (cons d nil) d) days))
      (goto-char (match-end 0)))
    (mhc-logic/set-day logicinfo (nreverse days)))) ;; xxxxx


(defun mhc-logic-parse-old-style-date (logicinfo)
  "X-SC-Date: ヘッダの日付部分を解析する関数"
  (if (looking-at mhc-logic/space-regexp)
      (goto-char (match-end 0)))
  (let (month)
    (if (and (mhc-logic/looking-at mhc-logic/old-style-date-regexp)
             (setq month (cdr (assoc (downcase (match-string 2))
                                     mhc-logic/month-alist))))
        (let ((year (string-to-number (match-string 3))))
          (mhc-logic/set-day
           logicinfo
           (cons (mhc-date-new (cond ((< year 69)
                                      (+ year 2000))
                                     ((< year 1000)
                                      (+ year 1900))
                                     (t year))
                               month
                               (string-to-number (match-string 1)))
                 (mhc-logic/day logicinfo)))
          (goto-char (match-end 0)))
      (error "Parse ERROR !!!(at X-SC-Date:)"))))


(defun mhc-logic-parse-cond (logicinfo)
  "X-SC-Cond: ヘッダを解析する関数"
  (let (sexp day-of-month week-of-month day-of-week month)
    (if (looking-at mhc-logic/space-regexp)
        (goto-char (match-end 0)))
    (while (not (eobp))
      (cond
       ;; 何日目
       ((mhc-logic/looking-at mhc-logic/day-of-month-regexp)
        (setq day-of-month
              (cons (list 'mhc-logic/condition-day-of-month (string-to-number (match-string 1)))
                    day-of-month)))
       ;; 何週目
       ((mhc-logic/looking-at mhc-logic/week-of-month-regexp)
        (setq week-of-month
              (cons (nth 2 (assoc (downcase (match-string 1))
                                  mhc-logic/week-of-month-alist))
                    week-of-month)))
       ;; 曜日
       ((mhc-logic/looking-at mhc-logic/day-of-week-regexp)
        (setq day-of-week
              (cons (list 'mhc-logic/condition-day-of-week
                          (cdr (assoc (downcase (match-string 1))
                                      mhc-logic/day-of-week-alist)))
                    day-of-week)))
       ;; 月
       ((mhc-logic/looking-at mhc-logic/month-regexp)
        (setq month
              (cons (list 'mhc-logic/condition-month
                          (cdr (assoc (downcase (match-string 1))
                                      mhc-logic/month-alist)))
                    month)))
       (t ;; 解釈できない要素の場合
        (error "Parse ERROR !!!(at X-SC-Cond:)")))
      (goto-char (match-end 0)))
    (mapcar (lambda (s)
              (set s (if (symbol-value s)
                         (if (= 1 (length (symbol-value s)))
                             (car (symbol-value s))
                           (cons 'or (nreverse (symbol-value s)))))))
            '(day-of-month week-of-month day-of-week month))
    (setq sexp (cond
                ((and week-of-month day-of-week) `(and ,week-of-month ,day-of-week))
                (week-of-month week-of-month)
                (day-of-week day-of-week)))
    (if day-of-month (setq sexp (if sexp (list 'or day-of-month sexp) day-of-month)))
    (if month (setq sexp (if sexp (list 'and month sexp) month)))
    (if sexp (mhc-logic/set-and logicinfo (cons sexp (mhc-logic/and logicinfo))))))


(defun mhc-logic-parse-duration (logicinfo)
  "X-SC-Duration: ヘッダを解析する関数"
  (let (sexp)
    (if (looking-at mhc-logic/space-regexp)
        (goto-char (match-end 0)))
    (while (not (eobp))
      (setq sexp
            (cons (cond
                   ((mhc-logic/looking-at mhc-logic/day-regexp
                                          "-" mhc-logic/day-regexp)
                    (list 'mhc-logic/condition-duration
                          (mhc-date-new (string-to-number (match-string 1))
                                        (string-to-number (match-string 2))
                                        (string-to-number (match-string 3)))
                          (mhc-date-new (string-to-number (match-string 4))
                                        (string-to-number (match-string 5))
                                        (string-to-number (match-string 6)))))
                   ((mhc-logic/looking-at mhc-logic/day-regexp "-")
                    (list 'mhc-logic/condition-duration-begin
                          (mhc-date-new (string-to-number (match-string 1))
                                        (string-to-number (match-string 2))
                                        (string-to-number (match-string 3)))))
                   ((mhc-logic/looking-at "-" mhc-logic/day-regexp)
                    (list 'mhc-logic/condition-duration-end
                          (mhc-date-new (string-to-number (match-string 1))
                                        (string-to-number (match-string 2))
                                        (string-to-number (match-string 3)))))
                   (t ; それ以外の場合
                    (error "Parse ERROR !!!(at X-SC-Duration:)")))
                  sexp))
      (goto-char (match-end 0)))
    (if sexp
        (mhc-logic/set-and logicinfo (cons (if (= 1 (length sexp))
                                               (car sexp)
                                             (cons 'or (nreverse sexp)))
                                           (mhc-logic/and logicinfo))))))

;; Need to be deleted.
(defun mhc-logic-parse-todo (logicinfo)
  (if (looking-at mhc-logic/space-regexp)
      (goto-char (match-end 0)))
  (let ((content (buffer-substring
                  (point) (progn (skip-chars-forward "0-9") (point)))))
    (if (looking-at mhc-logic/space-regexp)
        (goto-char (match-end 0)))
    (if (eobp)
        (mhc-logic/set-todo logicinfo (string-to-number content))
      (error "Parse ERROR !!!(at X-SC-Todo:)"))))


(defun mhc-logic-compile-file (record)
  "日付を指定されたときに、関係するスケジュールを選びだすためのS式を生成する"
  (let ((sexp) (schedules (mhc-record-schedules record))
        (byte-compile-warnings))
    (while schedules
      (setq sexp (cons (mhc-logic/compile-schedule (car schedules)) sexp)
            schedules (cdr schedules)))
    (setq sexp (delq nil sexp))
    (mhc-record-set-sexp
     record
     (if sexp
         (let (year month day day-of-month day-of-week week-of-month last-week todo)
           (byte-compile
            (list 'lambda ()
                  (if (= 1 (length sexp))
                      (car sexp)
                    (cons 'or (nreverse sexp))))))))))


(defun mhc-logic/compile-schedule (schedule)
  "mhc-logic-compile-file の下請け関数"
  (let* ((logicinfo (mhc-schedule-condition schedule)) sexp)
    ;; 日付による例外条件とそれ以外の条件を結合した論理式を生成する
    (setq sexp
          (nreverse
           (delq nil
                 (cons (let ((and (mhc-logic/and logicinfo)))
                         (if and
                             (if (= 1 (length and))
                                 (list (car and) t)
                               (list (cons 'and (reverse and)) t))))
                       (mapcar (lambda (day)
                                 (if (consp day)
                                     `((mhc-logic/condition-day ,(car day)) nil)
                                   `((mhc-logic/condition-day ,day) t)))
                               (mhc-logic/day logicinfo))))))
    (if sexp
        (progn
          ;; 条件の数によって、条件式を最適化しておく
          (setq sexp (if (= 1 (length sexp))
                         (if (nth 1 (car sexp))
                             (car (car sexp))
                           `(not ,(car (car sexp))))
                       (cons 'cond sexp)))
          ;; TODOに基づく条件を加える
          (setq sexp (if (mhc-logic-todo logicinfo)
                         `(if todo t ,sexp)
                       `(if todo nil ,sexp))))
      (if (mhc-logic-todo logicinfo)
          (setq sexp 'todo)))
    ;; この中間形式を保存しておく
    (mhc-logic/set-intermediate logicinfo sexp)
    ;; 中間形式を展開する
    (mhc-logic/set-sexp logicinfo
                        (if sexp (mhc-logic/macroexpand
                                  `(if ,sexp ,schedule))))))


(defun mhc-logic/macroexpand (sexp)
  "部分式に遡ってマクロを展開する関数"
  (macroexpand
   (if (listp sexp)
       (mapcar (function mhc-logic/macroexpand) sexp)
     sexp)))



;;----------------------------------------------------------------------
;;              mhc-logic-record-to-slot
;;----------------------------------------------------------------------

(defun mhc-logic-record-to-slot (record)
  "Return appropriate slot key, ( YEAR . MONTH ), for RECORD."
  (let ((schedules (mhc-record-schedules record))
        pre-month cur-month)
    (while (and schedules
                (not (mhc-logic-todo (mhc-schedule-condition (car schedules))))
                (setq cur-month
                      (mhc-logic/check-sexp-range
                       (mhc-schedule-condition (car schedules))))
                (if pre-month
                    (equal pre-month cur-month)
                  (setq pre-month cur-month)))
      (setq schedules (cdr schedules)))
    (if schedules (cons nil nil) cur-month)))


(defun mhc-logic/day-to-slot (day)
  "Generate slot key by DAY, which represents the number of days from 1970/01/01,"
  (mhc-day-let day (cons year month)))


(defun mhc-logic/check-sexp-range (logicinfo)
  "Estimate appropriate slot for LOGICINFO, with macro expression."
  (let (duration-begin duration-end day-list month-list require-duration)
    (mhc-logic/check-sexp-range-internal (mhc-logic/intermediate logicinfo))
    (if (or (> (length month-list) 1)
            (if require-duration
                (or (not duration-begin)
                    (not duration-end)))
            (progn
              (if day-list (setq day-list (sort day-list '<)))
              (not (equal
                    (setq duration-begin
                          (if day-list
                              (mhc-logic/day-to-slot
                               (if duration-begin
                                   (min (car day-list) duration-begin)
                                 (car day-list)))))
                    (if day-list
                        (mhc-logic/day-to-slot
                         (if duration-end
                             (max (nth (1- (length day-list)) day-list) duration-end)
                           (nth (1- (length day-list)) day-list))))))))
        '(nil . nil)
      duration-begin)))


(eval-when-compile
  (defvar day-list)
  (defvar duration-begin)
  (defvar duration-end)
  (defvar month-list)
  (defvar require-duration))

(defun mhc-logic/check-sexp-range-internal (sexp)
  "Recursive subroutine of mhc-logic/check-sexp-range."
  (if (listp sexp)
      (cond
       ((eq (car sexp) 'mhc-logic/condition-duration)
        (if (or (not duration-begin)
                (< (nth 1 sexp) duration-begin))
            (setq duration-begin (nth 1 sexp)))
        (if (or (not duration-end)
                (> (nth 1 sexp) duration-end))
            (setq duration-end (nth 2 sexp))))
       ((eq (car sexp) 'mhc-logic/condition-duration-begin)
        (if (or (not duration-begin)
                (< (nth 1 sexp) duration-begin))
            (setq duration-begin (nth 1 sexp))))
       ((eq (car sexp) 'mhc-logic/condition-duration-end)
        (if (or (not duration-end)
                (> (nth 1 sexp) duration-end))
            (setq duration-end (nth 1 sexp))))
       ((eq (car sexp) 'mhc-logic/condition-day)
        (setq day-list (cons (nth 1 sexp) day-list)))
       ((eq (car sexp) 'mhc-logic/condition-month)
        (or (memq (nth 1 sexp) month-list)
            (setq month-list (cons (nth 1 sexp) month-list)))
        (setq require-duration t))
       ((eq (car sexp) 'mhc-logic/condition-day-of-week)
        (setq require-duration t))
       ((eq (car sexp) 'mhc-logic/condition-day-of-month)
        (setq require-duration t))
       (t
        (while sexp
          (mhc-logic/check-sexp-range-internal (car sexp))
          (setq sexp (cdr sexp)))))))


; (defun mhc-logic-occur-multiple-p (logicinfo)
;   "If LOGICINFO occurs multiple times, return t."
;   (let (duration-begin duration-end day-list month-list require-duration)
;     (mhc-logic/check-sexp-range-internal (mhc-logic/intermediate logicinfo))
;     (if (or duration-begin
;           duration-end
;           month-list
;           (> (length day-list) 1))
;       t)))

;; rough (but safety) check  -- nom
(defun mhc-logic-occur-multiple-p (logicinfo)
  "If LOGICINFO occurs multiple times, return t."
  (if (or (mhc-logic/and logicinfo)
          (> (length (mhc-logic/day logicinfo)) 1))
      t))

(provide 'mhc-logic)

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

;;; mhc-logic.el ends here
