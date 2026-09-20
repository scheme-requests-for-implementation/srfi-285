;;; SPDX-FileCopyrightText: 2024-2026 Arvydas Silanskas
;;; SPDX-License-Identifier: MIT
;;; SPDX-FileCopyrightText: 2026 Peter McGoron

(define-record-type <dt>
  (make-delta years months weeks days hours minutes seconds)
  time-delta?
  (years time-delta-years)
  (months time-delta-months)
  (weeks time-delta-weeks)
  (days time-delta-days)
  (hours time-delta-hours)
  (minutes time-delta-minutes)
  (seconds time-delta-seconds))

(define (years-delta v)
    (unless (integer? v)
        (error "years-delta called with invalid parameters" v))
    (make-delta v 0 0 0 0 0 0))

(define (months-delta v)
    (unless (integer? v)
        (error "months-delta called with invalid parameters" v))
    (make-delta 0 v 0 0 0 0 0))

(define (weeks-delta v)
    (unless (integer? v)
        (error "weeks-delta called with invalid parameters" v))
    (make-delta 0 0 v 0 0 0 0))

(define (days-delta v)
    (unless (integer? v)
        (error "days-delta called with invalid parameters" v))
    (make-delta 0 0 0 v 0 0 0))

(define (hours-delta v)
    (unless (integer? v)
        (error "hours-delta called with invalid parameters" v))
    (make-delta 0 0 0 0 v 0 0))

(define (minutes-delta v)
    (unless (integer? v)
        (error "minutes-delta called with invalid parameters" v))
    (make-delta 0 0 0 0 0 v 0))

(define (seconds-delta v)
    (unless (exact? v)
        (error "seconds-delta called with invalid parameters" v))
    (make-delta 0 0 0 0 0 0 v))

(define (time-delta+ . dts)
    (define (sum-component getter)
        (fold (lambda (dt sum) (+ sum (getter dt)))
              0
              dts))
    (for-each (lambda (obj)
                (unless (time-delta? obj)
                    (error "dt+ called with invalid parameters" dts)))
               dts)
    (make-delta
     (sum-component time-delta-years)
     (sum-component time-delta-months)
     (sum-component time-delta-weeks)
     (sum-component time-delta-days)
     (sum-component time-delta-hours)
     (sum-component time-delta-minutes)
     (sum-component time-delta-seconds)))

(define (time-delta-negate dt)
    (unless (time-delta? dt)
        (error "time-delta-negate called with invalid parameters" dt))
    (make-delta
     (- (time-delta-years dt))
     (- (time-delta-months dt))
     (- (time-delta-weeks dt))
     (- (time-delta-days dt))
     (- (time-delta-hours dt))
     (- (time-delta-minutes dt))
     (- (time-delta-seconds dt))))

(define (date+ date time-delta)
    (unless (and (date? date)
                 (time-delta? time-delta)
                 (zero? (time-delta-hours time-delta))
                 (zero? (time-delta-minutes time-delta))
                 (zero? (time-delta-seconds time-delta)))
        (date-error "time-delta+ called with invalid parameters" date time-delta))
    (let*-values
        (((y m d) (date-ymd date))
         ((y+) (time-delta-years time-delta))
         ((m+) (time-delta-months time-delta))
         ((w+) (time-delta-weeks time-delta))
         ((d+) (time-delta-days time-delta))
         ((y) (+ y y+))
         ((y m) (add-months y m m+))
         ((d) (let* ((leap? (leap-year? y))
                     (max-days (days-in-month leap? m values error)))
                (min d max-days)))
         ((date*) (add-days y m d (+ (* 7 w+) d+))))
      date*))

;; adds months, performs carry over if necessary, returns (values year month)
(define (add-months year month month+)
    (let*-values
        (((m) (+ (- month 1) month+))
         ((y+ m) (truncate/ m 12))
         ((y+ m) (if (< m 0)
                     (values (- y+ 1) (+ m 12))
                     (values y+ m)))
         ((m) (+ 1 m)))
      (values (+ year y+) m)))

;; adds days, performs carry over if necessary, returns date
(define (add-days year month day day+)
    (if (= day+ 0)
        (make-date year month day)
        (let* ((date (make-date year month day))
                   (rtd (date->rata-die date))
                   (rtd (+ day+ rtd))
                   (date (rata-die->date rtd)))
              date)))
