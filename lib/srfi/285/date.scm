;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-FileCopyrightText: 2024-2026 Arvydas Silanskas
;;; SPDX-License-Identifier: MIT

;;; NOTE: Depending on the use case, it might be better for the data
;;; datatype to use Rata Die as its internal representation, instead
;;; of proleptic Gregorian.
(define-record-type <date>
    (make-date* year month day)
    date?
    (year date-year)
    (month date-month)
    (day date-day))

(define (valid-date? date)
  (let* ((y (date-year date))
         (m (date-month date))
         (d (date-day date)))
    (and (integer? y)
         (integer? m)
         (integer? d)
         (<= 1 m 12)
         (days-in-month (leap-year? y) m
                        (lambda (d*) (<= 1 d d*))
                        (lambda () #f)))))

(define (make-date year month day)
  (let ((d (make-date* year month day)))
    (unless (valid-date? d)
      (error "make-date called with invalid parameters" year month day))
    d))

(define (date-ymd d)
  (unless (date? d)
    (error "date-ymd called with invalid parameters" d))
  (values (date-year d) (date-month d) (date-day d)))

(define (date-weekday date)
  (unless (date? date)
    (error "date-weekday called with invalid parameters" date))
  (call-with-values
    (lambda () (date-ymd date))
    (lambda (year month day)
      (let ((num (zeller-congruence year month day)))
        ;; Zeller's congruence returns 0 as saturday, need to shift so that 0 is sunday
        (floor-remainder (+ num 6) 7)))))

(define (date-iso-weekday date)
  (unless (date? date)
    (error "date-iso-weekday called with invalid parameters" date))
  (let ((weekday (date-weekday date)))
    ;; conver from [0 sunday,  6  saturday] to [1 monday, 7 sunday]
    (if (= 0 weekday) 7 weekday)))

;; https://en.wikipedia.org/wiki/Zeller%27s_congruence
(define (zeller-congruence year month day)
  ;; variables named to match formula in wiki page
  (let* ((q day)
         (m (if (>= month 3) month (+ month 12)))
         (adjYear (if (>= month 3) year (- year 1)))
         (K (floor-remainder adjYear 100))
         (J (floor-quotient adjYear 100)))
    (floor-remainder
     (+ q
        (floor-quotient (* 13 (+ m 1))
                        5)
        K
        (floor-quotient K 4)
        (floor-quotient J 4)
        (- (* 2 J)))
     7)))

(define (days-since-year-start date)
  (define-values (year month day) (date-ymd date))
  (define is-leap (leap-year? year))
  (let loop ((days 0)
             (i 1))
    (if (= i month)
        (+ days day)
        (days-in-month is-leap i
                       (lambda (d) (loop (+ days d) (+ 1 i)))
                       (lambda () (error "Internal date-time implementation bug"))))))

(define (apply-days-after-year-start year days)
  (define is-leap (leap-year? year))
  (let loop ((days days)
             (month 1))
    (days-in-month is-leap month
                   (lambda (d) (if (> days d)
                                   (loop (- days d)
                                         (+ 1 month))
                                   (make-date year month days)))
                   (lambda () (error "Internal date-time implementation bug")))))

(define (date-iso-week date)
  (unless (date? date)
    (error "date-iso-week called with invalid parameters" date))
  (call-with-values
    (lambda () (date-iso-week* date))
    (lambda (year week) week)))

(define (date-iso-week-year date)
  (unless (date? date)
    (error "date-iso-week-year called with invalid parameters" date))
  (call-with-values
    (lambda () (date-iso-week* date))
    (lambda (year week) year)))

;; returns two values -- week year and week number for the given date
(define (date-iso-week* date)
  ;; 3 cases to consider
  ;; if it's <= 3rd jan, it might belong to last year's week year
  ;; if it's >= 29 dec, it might belong to next year's week year
  ;; or it is this year's week year
  (define-values (year month day) (date-ymd date))
  (define weekday (date-iso-weekday date))
  (define year-offset
    (cond
      ((and (= month 1) (<= day 3) (>= (- weekday day) 4)) -1)
      ((and (= month 12) (>= day 29) (>= (- day weekday) 28)) 1)
      (else 0)))

  (case year-offset
    ((1)
     (values (+ 1 year) 1))
    ((-1)
     (let* ((day-of-week-on-jan1 (date-iso-weekday (make-date* (- (date-year date) 1) 1 1)))
            ;; first week is the one which has first thursday in the new year
            ;; offset is amount to add to `days` to make relative to monday of first week
            (offset (if (>= 5 day-of-week-on-jan1)
                        (- day-of-week-on-jan1 1)
                        (- 1 day-of-week-on-jan1)))
            (day-count (+ offset
                          (if (leap-year? (- year 1)) 366 365)
                          day)))
       (values (- year 1)
               (+ 1 (floor-quotient day-count 7)))))
    (else
      (let* ((day-of-week-on-jan1 (date-iso-weekday (make-date* (date-year date) 1 1)))
             ;; first week is the one which has first thursday in the new year
             ;; offset is amount to add to `days` to make relative to monday of first week (which could be in prev year)
             (offset (if (>= 5 day-of-week-on-jan1)
                         (- day-of-week-on-jan1 1)
                         (- 1 day-of-week-on-jan1)))
             (day-count (+ (days-since-year-start date) offset)))
        (values year
                (+ 1 (floor-quotient day-count 7)))))))

(define (left-pad str c min-length)
  (define pad-size (max 0 (- min-length (string-length str))))
  (if (> pad-size 0)
      (let ((pad (make-string pad-size c)))
        (string-append pad str))
      str))

(define (date->iso-8601 date)
  (unless (date? date)
    (error "date->iso-8601 called with invalid parameters" date))
  (let-values (((y m d) (date-ymd date)))
    (string-append
      (if (< y 0) "-" "")
      (left-pad (number->string (abs y)) #\0 4)
      "-"
      (left-pad (number->string m) #\0 2)
      "-"
      (left-pad (number->string d) #\0 2))))

(define (make-transitive binary-predicate)
  (lambda (x y . rest)
    (let loop ((x x) (y y) (rest rest))
      (and (binary-predicate x y)
           (or (null? rest)
               (loop y (car rest) (cdr rest)))))))

(define (%date=? date1 date2)
  (unless (and (date? date1) (date? date2))
    (error "date=? called with invalid parameters" date1 date2))
  (let-values (((y1 m1 d1) (date-ymd date1))
               ((y2 m2 d2) (date-ymd date2)))
    (and (= y1 y2) (= m1 m2) (= d1 d2))))
(define date=? (make-transitive %date=?))

(define (%date<? date1 date2)
  (unless (and (date? date1) (date? date2))
    (error "date<? called with invalid parameters" date1 date2))
  (let-values (((y1 m1 d1) (date-ymd date1))
               ((y2 m2 d2) (date-ymd date2)))
    (cond
      ((< y1 y2) #t)
      ((> y1 y2) #f)
      ((< m1 m2) #t)
      ((> m1 m2) #f)
      (else (< d1 d2)))))
(define date<? (make-transitive %date<?))

(define (%date<=? date1 date2)
  (unless (and (date? date1) (date? date2))
    (error "date<=? called with invalid parameters" date1 date2))
  (let-values (((y1 m1 d1) (date-ymd date1))
               ((y2 m2 d2) (date-ymd date2)))
    (cond
      ((< y1 y2) #t)
      ((> y1 y2) #f)
      ((< m1 m2) #t)
      ((> m1 m2) #f)
      (else (<= d1 d2)))))
(define date<=? (make-transitive %date<=?))

(define (%date>? date1 date2)
  (unless (and (date? date1) (date? date2))
    (error "date>? called with invalid parameters" date1 date2))
  (let-values (((y1 m1 d1) (date-ymd date1))
               ((y2 m2 d2) (date-ymd date2)))
    (cond
      ((> y1 y2) #t)
      ((< y1 y2) #f)
      ((> m1 m2) #t)
      ((< m1 m2) #f)
      (else (> d1 d2)))))
(define date>? (make-transitive %date>?))

(define (%date>=? date1 date2)
  (unless (and (date? date1) (date? date2))
    (error "date>=? called with invalid parameters" date1 date2))
  (let-values (((y1 m1 d1) (date-ymd date1))
               ((y2 m2 d2) (date-ymd date2)))
    (cond
      ((> y1 y2) #t)
      ((< y1 y2) #f)
      ((> m1 m2) #t)
      ((< m1 m2) #f)
      (else (>= d1 d2)))))
(define date>=? (make-transitive %date>=?))

;; helpers for converting between iso date and rata / julian
(define days/4y   (+ (* 4 365) 1))
(define days/100y (+ (* 100 365) 24))
(define days/400y (+ (* 4 days/100y) 1))

;; handles only dates with positive year
(define (date->rata-die* date)
  (define (rd-for-year year)
    (let* (;; subtract the precalculated days of current year; those will be added by days-since-year-start
           (curr-year-adjust (if (leap-year? year) -366 -365))
           (part400s (* days/400y (floor-quotient year 400)))
           (year (floor-remainder year 400))
           (part100s (* days/100y (floor-quotient year 100)))
           (year (floor-remainder year 100))
           (part4s (* days/4y (floor-quotient year 4)))
           (year (floor-remainder year 4))
           (rest (* year 365)))
        (+ part400s part100s part4s rest curr-year-adjust)))
  (let ((year (date-year date)))
    (+ (rd-for-year year) (days-since-year-start date))))

(define (date->rata-die date)
  (unless (date? date)
    (error "date->rata-die called with invalid parameters" date))
  (let ((year (date-year date)))
    (if (>= year 0)
        (date->rata-die* date)
        ;; slowpath for negative date: compute n such that n*400 + year > 0
        ;; then the result is difference between rd of n*400 + 1 and n*400 + year
        (let* ((month (date-month date))
               (day (date-day date))
               (n (+ 1 (floor-quotient (abs year) 400)))
               (rd1 (date->rata-die* (make-date (+ year (* n 400)) month day)))
               (rd2 (date->rata-die* (make-date (+ 1 (* n 400)) 1 1))))
          (+ 1 (- rd1 rd2))))))

;; handles only positive RD
(define (rata-die->date* rd)
  (define (floor-quotient/capped num denom max)
      (let ((v (floor-quotient num denom)))
        (min v max)))
  (define (floor-remainder/capped num denom max)
      (let* ((v (floor-quotient num denom))
             (v (min v max)))
        (- num (* v denom))))
  (let* ((rd (- rd 1))
         (part400s (* 400 (floor-quotient rd days/400y)))
         (rd (floor-remainder rd days/400y))
         (part100s (* 100 (floor-quotient/capped rd days/100y 3)))
         (rd (floor-remainder/capped rd days/100y 3))
         (part4s (* 4 (floor-quotient/capped rd days/4y 24)))
         (rd (floor-remainder/capped rd days/4y 24))
         (rest (floor-quotient/capped rd 365 3))
         (rd (floor-remainder/capped rd 365 3))
         (year (+ part400s part100s part4s rest)))
    (apply-days-after-year-start (+ 1 year) (+ 1 rd))))

(define (rata-die->date rd)
  (unless (integer? rd)
    (error "rata-die->date called with invalid parameters" rd))
  (if (>= rd 1)
      (rata-die->date* rd)
      ;; slowpath for rd less than 1: compute n such that n*days/400y + rd > 0
      ;; then the result is computing date for n*days/400y + rd and
      ;; moving result's year value backwards by n * 400
      (let* ((n (+ 1 (floor-quotient (abs rd) days/400y)))
             (date* (rata-die->date* (+ (* n days/400y) rd)))
             (year (- (date-year date*) (* n 400))))
        (make-date* year (date-month date*) (date-day date*)))))

(define mjd-rd-offset (- 2400000 1721424))
(define unix-epoch-rd (date->rata-die (make-date 1970 1 1)))

(define (date->mjd date)
  (unless (date? date)
    (error "date->mjd called with invalid parameters" date))
  (- (date->rata-die date) mjd-rd-offset))

(define (mjd->date mjd)
  (unless (integer? mjd)
    (error "mjd->date called with invalid parameters" mjd))
  (rata-die->date (+ mjd-rd-offset mjd)))
