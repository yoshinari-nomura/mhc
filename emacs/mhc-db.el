;;; -*- mode: Emacs-Lisp; coding: utf-8 -*-

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Created: 2000/05/01
;; Revised: $Date$


;;; Commentary:

;; This file is a part of MHC, and includes functions to manipulate
;; database of schedules.


;;; Code:

(require 'mhc-day)
(require 'mhc-slot)
(require 'mhc-schedule)

(defun mhc-db/get-sexp-list-for-month (year month)
  "指定された月のスケジュールを探索するときに、評価するべきS式のリストを得る"
  (mapcar
   (lambda (f) (mhc-record-sexp f))
   (apply (function nconc)
          (delq nil
                (mapcar (lambda (x)
                          (and x
                               (setq x (mhc-slot-records x))
                               (copy-sequence x)))
                        (list
                         (mhc-slot-get-month-schedule (cons year month))
                         (mhc-slot-get-intersect-schedule)
                         (mhc-slot-get-constant-schedule)))))))


(defun mhc-db/eval-for-duration (from to &optional todo) "\
ある期間 FROM〜TO に対してスケジュールを探索する
FROM, TO は 1970/01/01 からの経過日数を用いて指定"
  (let (list new)
    (mhc-day-let from
      (let* ((day from)
             (week-of-month (/ (1- day-of-month) 7))
             ;; FIXME: mhc-date.el の内部関数を呼び出している。
             (last-day-of-month (mhc-date/last-day-of-month year month))
             (last-week (> 7 (- last-day-of-month day-of-month)))
             (sexp-list (mhc-db/get-sexp-list-for-month year month)))
        (while (<= day to)
          (setq new (mhc-day-new day year month day-of-month day-of-week))
          (mhc-day-set-schedules new (delq nil
                                           (mapcar (lambda (sexp)
                                                     (and sexp
                                                          (funcall sexp)))
                                                   sexp-list)))
          (setq list (cons new list)
                day (1+ day)
                day-of-month (1+ day-of-month)
                day-of-week (% (1+ day-of-week) 7))
          (if (> day-of-month last-day-of-month)
              ;; 1ヶ月を超えて連続した探索を行う場合
              (setq month (1+ (% month 12))
                    year (if (= 1 month) (1+ year) year)
                    day-of-month 1
                    week-of-month 0
                    last-week nil
                    ;; FIXME: mhc-date.el の内部関数を呼び出している。
                    last-day-of-month (mhc-date/last-day-of-month year month)
                    sexp-list (mhc-db/get-sexp-list-for-month year month))
            ;; 週末毎の処理
            (setq week-of-month (/ (1- day-of-month) 7))
            (and (not last-week)
                 (> 7 (- last-day-of-month day-of-month))
                 (setq last-week t)))))
      (nreverse list))))


(defun mhc-db/eval-for-month (year month &optional todo)
  "指定された月のスケジュールを探索"
  (let ((from (mhc-date-new year month 1)))
    (mhc-db/eval-for-duration from (mhc-date-mm-last from) todo)))

(defun mhc-db/holiday-p (dayinfo)
  (catch 'holiday
    (let ((schedules (mhc-day-schedules dayinfo)))
      (while schedules
        (if (mhc-schedule-in-category-p (car schedules) "holiday")
            (throw 'holiday t))
        (setq schedules (cdr schedules))))))


(defun mhc-db/sort-schedules-by-time (dayinfo)
  (if (mhc-day-schedules dayinfo)
      (let (time)
        (mapcar
         (function cdr)
         (sort (mapcar
                (lambda (schedule)
                  (cons (cond
                         ((setq time (mhc-schedule-time-begin schedule)) time)
                         ((mhc-schedule-in-category-p schedule "holiday")
                          (mhc-day-set-holiday dayinfo t)
                          -1)
                         (t 0))
                        schedule))
                (mhc-day-schedules dayinfo))
               (lambda (a b) (< (car a) (car b))))))))


(defun mhc-db-scan (from to &optional nosort)
  (let ((list (mhc-db/eval-for-duration from to)))
    (let ((days list))
      (if nosort
          ;; 所用の開始時間に基づく並べ替えは行わずに、祝日のチェックのみを行う
          (while days
            (mhc-day-set-holiday (car days) (mhc-db/holiday-p (car days)))
            (setq days (cdr days)))
        ;; 所用の開始時間に基づく並べ替えも同時に行う
        (while days
          (mhc-day-set-schedules (car days) (mhc-db/sort-schedules-by-time (car days)))
          (setq days (cdr days)))))
    list))


(defun mhc-db-scan-month (year month &optional nosort)
  (let ((list (mhc-db/eval-for-month year month)))
    (let ((days list))
      (if nosort
          ;; 所用の開始時間に基づく並べ替えは行わずに、祝日のチェックのみを行う
          (while days
            (mhc-day-set-holiday (car days) (mhc-db/holiday-p (car days)))
            (setq days (cdr days)))
        ;; 所用の開始時間に基づく並べ替えも同時に行う
        (while days
          (mhc-day-set-schedules (car days) (mhc-db/sort-schedules-by-time (car days)))
          (setq days (cdr days)))))
    list))


(defun mhc-db-scan-todo (day)
  (mapcar 'cdr
          (sort (mapcar
                 (lambda (schedule)
                   (cons (mhc-schedule-priority schedule)
                         schedule))
                 (sort (mhc-day-schedules
                        (mhc-logic-eval-for-date
                         (mhc-day-let day
                           (mhc-db/get-sexp-list-for-month year month))
                         day 'todo))
                       (lambda (x y)
                         (< (or (mhc-schedule-todo-deadline x) 65535)
                            (or (mhc-schedule-todo-deadline y) 65535)))))
                (lambda (a b)
                  (if (and (null (car a)) (car b))
                      nil
                    (if (and (null (car b)) (car a))
                        t
                      (if (and (null (car b)) (null (car a)))
                          nil
                        (> (car a) (car b)))))))))

(defun mhc-db-scan-memo (day)
  "行方不明の schedule の取得"
  (let ((schedules (mapcar
                    (lambda (f) (car (mhc-record-schedules f)))
                    (apply (function nconc)
                           (delq nil
                                 (mapcar (lambda (x)
                                           (and x
                                                (setq x (mhc-slot-records x))
                                                (copy-sequence x)))
                                         (list (mhc-slot-get-intersect-schedule)))))))
        schedule memos)
    (while (setq schedule (car schedules))
      (unless (or (mhc-logic/day (mhc-schedule-condition schedule))
                  (mhc-logic/and (mhc-schedule-condition schedule))
                  (and mhc-insert-todo-list
                       (mhc-schedule-in-category-p schedule "todo")))
        (setq memos (cons schedule memos)))
      (setq schedules (cdr schedules)))
    (mapcar 'cdr
            (sort (mapcar (lambda (x)
                            (cons (mhc-schedule-priority x) x))
                          memos)
                  (lambda (a b)
                    (if (and (null (car a)) (car b))
                        nil
                      (if (and (null (car b)) (car a))
                          t
                        (if (and (null (car b)) (null (car a)))
                            nil
                          (> (car a) (car b))))))))))


(defun mhc-db-add-record-from-buffer (record buffer &optional force-refile)
  (let* ((slot (mhc-logic-record-to-slot record))
         (directory (and slot (mhc-slot-key-to-directory slot)))
         (old-record))
    (unless slot (error "Cannot get schedule slot"))
    (if (mhc-record-name record)
        ;; 既存のスケジュールを編集した場合
        (if (string= directory
                     (file-name-directory
                      (directory-file-name
                       (mhc-record-name record))))
            (setq old-record record)
          ;; スケジュール変更によって、ディレクトリの変更が必要な場合
          (setq old-record (mhc-record-copy record))
          (mhc-record-set-name record (mhc-misc-get-new-path directory)))
      ;; 新規のスケジュールを保存する場合
      (mhc-record-set-name record (mhc-misc-get-new-path directory)))
    (if (or force-refile
            (y-or-n-p (format
                       "Refile %s to %s "
                       (mhc-misc-sub (if old-record
                                         (mhc-record-name old-record) "")
                                     mhc-mail-path "+")
                       (mhc-misc-sub (mhc-record-name record)
                                     mhc-mail-path "+"))))
        (progn
          (mhc-record-write-buffer record buffer old-record)
          (if (and old-record
                   (not (eq record old-record)))
              (let* ((dir (file-name-directory
                           (directory-file-name
                            (mhc-record-name old-record))))
                     (slot (mhc-slot-directory-to-key dir)))
                (mhc-misc-touch-directory dir)
                (mhc-slot-update-cache slot 'remove old-record)))
          (mhc-misc-touch-directory directory)
          (mhc-slot-update-cache slot 'add record)
          t))))


(defun mhc-db-delete-file (record)
  (let* ((dir (file-name-directory (directory-file-name (mhc-record-name record))))
         (slot (mhc-slot-directory-to-key dir)))
    (mhc-record-delete record)
    (mhc-misc-touch-directory dir)
    (mhc-slot-update-cache slot 'remove record)))


;; FIXME: X-SC-Schedule ヘッダによって指定された子スケジュールに対する
;; 例外規則の追加が動作しない。
(defun mhc-db-add-exception-rule (original-record except-day)
  (let ((date-string (mhc-day-let except-day
                       (format "%04d%02d%02d" year month day-of-month))))
    (with-temp-buffer
      (mhc-draft-reedit-file (mhc-record-name original-record))
      (let (record dayinfo schedule)
        (while (setq record (mhc-parse-buffer)
                     dayinfo (mhc-logic-eval-for-date (list (mhc-record-sexp record)) except-day)
                     schedule (car (mhc-day-schedules dayinfo)))
          (save-restriction
            (narrow-to-region (mhc-schedule-region-start schedule)
                              (mhc-schedule-region-end schedule))
            (mhc-header-put-value
             "x-sc-day"
             (mapconcat 'identity
                        (cons (format "!%s" date-string)
                              (delete date-string
                                      (mhc-logic-day-as-string-list
                                       (mhc-schedule-condition schedule))))
                        " "))))
        (mhc-record-set-name record (mhc-record-name original-record))
        (mhc-db-add-record-from-buffer record (current-buffer))))))



(provide 'mhc-db)

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

;;; mhc-db.el ends here.
