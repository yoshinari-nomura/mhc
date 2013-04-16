;;; -*- mode: Emacs-Lisp; coding: utf-8 -*-

;; Author:  Yoshinari Nomura <nom@quickhack.net>,
;;          TSUCHIYA Masatoshi <tsuchiya@namazu.org>
;; Created: 1997/10/12
;; Revised: $Date: 2004/05/06 16:35:12 $


;;; Commentary:

;; This file is a part of MHC, and includes functions to manipulate
;; MHC-SCHEDULE structure.


;; About MHC-SCHEDULE structure:

;; Each MHC-SCHEDULE structure is a vector has a construction as
;; follows:
;;
;;     MHC-SCHEDULE ::= [ RECORD CONDITION SUBJECT LOCATION TIME ALARM CATEGORIES PRIORITY REGION ]
;;     RECORD       ::= MHC-RECORD
;;     CONDITION    ::= MHC-LOGIC
;;     SUBJECT      ::= string ( represents subject of schedule )
;;     LOCATION     ::= string ( represents location of schedule )
;;     TIME         ::= integer ( represents minutes of day from midnight )
;;     ALARM        ::= string
;;     CATEGORIES   ::= CATEGORY*
;;     CATEGORY     ::= string ( represents category of schedule )
;;     PRIORITY     ::= integer
;;     REGION       ::= ( START . END )
;;     START        ::= integer ( represents start point of headers of schedule )
;;     END          ::= integer ( represents end point of headers of schedule )


;;; Codes:
(defun mhc-schedule-new
  (record &optional condition subject location time alarm categories priority region recurrence-tag)
  "Constructor of MHC-SCHEDULE structure."
  (let ((new (vector record
                     (or condition (mhc-logic-new))
                     subject
                     location
                     time
                     alarm
                     categories
                     priority
                     (or region (cons nil nil))
                     recurrence-tag)))
    (mhc-record-set-schedules record (cons new (mhc-record-schedules record)))
    new))

(defsubst mhc-schedule-record (schedule)
  (if schedule (aref schedule 0)))
(defsubst mhc-schedule-condition (schedule)
  (if schedule (aref schedule 1)))
(defsubst mhc-schedule-subject (schedule)
  (if schedule (aref schedule 2)))
(defsubst mhc-schedule-location (schedule)
  (if schedule (aref schedule 3)))
(defsubst mhc-schedule-time (schedule)
  (if schedule (aref schedule 4)))
(defsubst mhc-schedule-alarm (schedule)
  (if schedule (aref schedule 5)))
(defsubst mhc-schedule-categories (schedule)
  (if schedule (aref schedule 6)))
(defsubst mhc-schedule-priority (schedule)
  (if schedule (aref schedule 7)))
(defsubst mhc-schedule-region (schedule)
  (if schedule (aref schedule 8)))
(defsubst mhc-schedule-recurrence-tag (schedule)
  (if schedule (aref schedule 9)))

(defmacro mhc-schedule-time-begin (schedule)
  `(car (mhc-schedule-time ,schedule)))
(defmacro mhc-schedule-time-end (schedule)
  `(cdr (mhc-schedule-time ,schedule)))
(defmacro mhc-schedule-region-start (schedule)
  `(car (mhc-schedule-region ,schedule)))
(defmacro mhc-schedule-region-end (schedule)
  `(cdr (mhc-schedule-region ,schedule)))

;; Need to be deleted.
(defsubst mhc-schedule-todo-lank (schedule)
  (if schedule
      (mhc-logic-todo (mhc-schedule-condition schedule))))

(defsubst mhc-schedule-todo-deadline (schedule)
  (and schedule
       (or (car (mhc-logic/day (mhc-schedule-condition schedule)))
           (nth 2 (assq
                   'mhc-logic/condition-duration
                   (mhc-logic/and
                    (mhc-schedule-condition schedule))))
           (cadr (assq
                  'mhc-logic/condition-duration-end
                  (mhc-logic/and
                   (mhc-schedule-condition schedule)))))))

(defmacro mhc-schedule/set-subject (schedule subject)
  `(aset ,schedule 2 ,subject))
(defmacro mhc-schedule/set-location (schedule location)
  `(aset ,schedule 3 ,location))
(defmacro mhc-schedule/set-time (schedule begin end)
  `(aset ,schedule 4 (cons ,begin ,end)))
(defmacro mhc-schedule/set-alarm (schedule alarm)
  `(aset ,schedule 5 ,alarm))
(defmacro mhc-schedule/set-categories (schedule categories)
  `(aset ,schedule 6 ,categories))
(defmacro mhc-schedule/set-priority (schedule priority)
  `(aset ,schedule 7 ,priority))
(defmacro mhc-schedule/set-region-start (schedule start)
  `(setcar (aref ,schedule 8) ,start))
(defmacro mhc-schedule/set-region-end (schedule end)
  `(setcdr (aref ,schedule 8) ,end))
(defmacro mhc-schedule/set-recurrence-tag (schedule tag)
  `(aset ,schedule 9 ,tag))


(defun mhc-schedule-append-default (schedule default)
  (or (mhc-schedule-subject schedule)
      (mhc-schedule/set-subject schedule (mhc-schedule-subject default)))
  (or (mhc-schedule-location schedule)
      (mhc-schedule/set-location schedule (mhc-schedule-location default)))
  (or (mhc-schedule-time schedule)
      (not (mhc-schedule-time default))
      (mhc-schedule/set-time schedule
                             (mhc-schedule-time-begin default)
                             (mhc-schedule-time-end default)))
  (or (mhc-schedule-alarm schedule)
      (mhc-schedule/set-alarm schedule (mhc-schedule-alarm default)))
  (or (mhc-schedule-categories schedule)
      (mhc-schedule/set-categories schedule (mhc-schedule-categories default)))
  (or (mhc-schedule-recurrence-tag schedule)
      (mhc-schedule/set-recurrence-tag schedule (mhc-schedule-recurrence-tag default))))


(defsubst mhc-schedule/time-to-string (minutes)
  (format "%02d:%02d" (/ minutes 60) (% minutes 60)))


(defun mhc-schedule-time-as-string (schedule)
  (let ((time (mhc-schedule-time schedule)))
    (cond
     ((and (car time) (cdr time))
      (concat (mhc-schedule/time-to-string (car time))
              "-"
              (mhc-schedule/time-to-string (cdr time))))
     ((car time)
      (mhc-schedule/time-to-string (car time)))
     ((cdr time)
      (concat "-" (mhc-schedule/time-to-string (cdr time))))
     (t ""))))


(defun mhc-schedule-subject-as-string (schedule)
  (or (mhc-schedule-subject schedule) "(none)"))


(defun mhc-schedule-categories-as-string (schedule)
  (let ((categories (mhc-schedule-categories schedule)))
    (if categories
        (mapconcat (function identity) categories " ")
      "")))


(defun mhc-schedule-in-category-p (schedule category)
  (and schedule
       (if (listp category)
           (catch 'found
             (while category
               (if (member (downcase (car category)) (mhc-schedule-categories schedule))
                   (throw 'found t))
               (setq category (cdr category))))
         (member (downcase category) (mhc-schedule-categories schedule)))))


(defun mhc-schedule-recurrence-tag-as-string (schedule)
  (or (mhc-schedule-recurrence-tag schedule) ""))

(provide 'mhc-schedule)

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

;;; mhc-schedule.el ends here.
