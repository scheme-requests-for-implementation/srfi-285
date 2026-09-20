;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define (month-week-day->rd year month week weekday)
  (let* ((weekday (if (zero? weekday)
                      7
                      weekday))
         (day (cond
                ((= week 5)
                 (last-in-month year month weekday))
                (else
                 (nth-in-month year month weekday week)))))
    (date->rata-die (make-date year month day))))

(define (zero-based-day->date year zero-based-day)
  (+ (date->rata-die (make-date year 1 1))
     zero-based-day))

(define (one-based-day->rd year one-based-day)
  ;; From POSIX: https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap08.html#tag_08
  ;; 
  ;; "That is, in all years-including leap years-February 28 is day 59
  ;; and March 1 is day 60."
  (if (<= one-based-day 59)
      (+ (date->rata-die (make-date year 1 1))
         (- one-based-day 1))
      (+ (date->rata-die (make-date year 3 1))
         (- one-based-day 60))))

(define (tz-offset->seconds offset)
  (* (if (tz-offset-sign-negative? offset)
         -1
         1)
     (+ (* (tz-offset-hour offset) 60 60)
        (* (tz-offset-minute offset) 60)
        (tz-offset-second offset))))

(define epoch
  (date->rata-die (make-date 1970 1 1)))

(define (transition-local year transition)
  ;; Calculate the transition time in local UNIX time.
  (let* ((date (tz-transition-date transition))
         (time (tz-transition-time transition))
         (rd (cond
               ((tz-julian-with-leap-day? date)
                (zero-based-day->rd year
                                    (tz-julian-with-leap-day:date date)))
               ((tz-julian-without-leap-day? date)
                (one-based-day->rd year
                                   (tz-julian-without-leap-day:date date)))
               ((tz-month-week-day? date)
                (month-week-day->rd year
                                    (tz-month-week-day:month date)
                                    (tz-month-week-day:week date)
                                    (tz-month-week-day:day date)))
               (else (error "internal error: invalid date" date))))
         (rd (- rd epoch))
         (time-seconds (tz-offset->seconds time)))
    (+ (* rd 60 60 24) time-seconds)))

(define (add-offset unix offset)
  (+ unix
     (* (time-delta-hours offset) 60 60)
     (* (time-delta-minutes offset) 60)
     (time-delta-seconds offset)))

(define (transition-unix offset year transition)
  (let ((local (transition-local year transition)))
    ;; Reverse the UTC offset.
    (add-offset local (time-delta-negate offset))))

(define (in-offset time year subindex dst->std std->dst
                   dst-offset std-offset)
  #| Check if the local time is in the subrange.

Year describes the year that the center DST and STD transitions occur.
The subindex will shift forward or backward one transition. For instance,
+1 will be the range between the STD transition of this year and the
DST transition of the next. +2 is equivalent to +1 to year, +0 to subindex.

DST->STD and STD->DST are functions that take a year and return the
appropriate time.

For a given year:

 subindex=-1                 subindex=0          subindex=1
                 [year start]                   [year end]
DST->STD              STD->DST        DST->STD                DST->STD

This code has to account for the fact that there are invalid local
timestamps. For example, 2:00 AM is an invalid timestamp during the
transition from EST to EDT. When it returns #f, then it has detected
that the local timestamp is impossible.
|#
  (let*-values (((year+ new-subindex) (truncate/ subindex 2))
                ((year) (+ year year+)))
    (case subindex
      ((-1)
       (let ((std-transition (dst->std (- year 1) std-offset))
             (dst-transition (std->dst year std-offset)))
         (cond
           ((< time (dst->std (- year 1) std-offset)) -1)
           ((< time (dst->std (- year 1) dst-offset)) #f)
           ((<= (std->dst year std-offset) time) 1)
           (else 0))))
      ((0)
       (cond
         ((< time (std->dst year std-offset)) -1)
         ((< time (std->dst year dst-offset)) #f)
         ((<= (dst->std year dst-offset) time) 1)
         (else 0)))
      ((1)
       (cond
         ((< time (dst->std year dst-offset)) -1)
         ((< time (dst->std year std-offset)) #f)
         ((<= (std->dst (+ year 1) std-offset) time) 1)
         (else 0)))
      (else (error "invalid subindex" subindex)))))

(define (calculate-year time)
  (let ((days-since-1970 (floor/ time (* 60 60 24))))
    (date-year (rata-die->date (+ days-since-1970 epoch)))))

(define (offset-rule time year subindex
                     dst->std std->dst
                     dst-offset std-offset
                     dst-designation std-designation)
  (let*-values (((year+ subindex) (truncate/ subindex 2))
                ((year) (+ year year+))
                ((change) (in-offset time year subindex
                                     dst->std std->dst
                                     dst-offset std-offset)))
    (if (eqv? change 0)
        (vector
         (case subindex
           ((0) (time-range (std->dst year dst-offset)
                            (dst->std year dst-offset)
                            dst-offset
                            #t
                            dst-designation))
           ((1) (time-range (dst->std year std-offset)
                            (std->dst (+ year 1) std-offset)
                            std-offset
                            #f
                            std-designation))
           ((-1) (time-range (dst->std (- year 1) std-offset)
                             (std->dst year std-offset)
                             std-offset
                             #f
                             std-designation))))
        '#())))

(define (find-an-offset-rule local-time dst->std std->dst
                             dst-offset std-offset)
  (let loop ((year (calculate-year local-time))
             (subindex 0))
    #;(write (list year subindex
                   local-time
                   ':
                   (dst->std (- year 1) dst-offset)
                   (std->dst year std-offset)
                   (dst->std year dst-offset)
                   (std->dst (+ year 1) std-offset)))
    #;(newline)
    (case (in-offset local-time year subindex
                     dst->std std->dst
                     dst-offset std-offset)
      ((0) (values year subindex))
      ((-1) (case subindex
              ((0) (loop year -1))
              ((-1) (loop (- year 1) 1))
              ((1) (loop year 0))))
      ((1) (case subindex
             ((0) (loop year 1))
             ((-1) (loop year 0))
             ((1) (loop (+ year 1) -1))))
      ((#f) (values year subindex))    ; Dummy return
      (else => (lambda (x)
                 (error "invalid adjustment" x))))))
