;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define (day-name->weekday name)
  (case name
    ((monday) 1)
    ((tuesday) 2)
    ((wednesday) 3)
    ((thursday) 4)
    ((friday) 5)
    ((saturday) 6)
    ((sunday) 7)
    (else (error "not a weekday" name))))

(define (days-in-month year month)
  (case month
    ((1 3 5 7 8 10 12) 31)
    ((4 6 9 11) 30)
    ((2) (if (leap-year? year)
             29
             28))
    (else (error "not a valid month" month))))

(define (first-in-month year month weekday)
  (let ((first-weekday (date-iso-weekday (make-date year month 1))))
    (cond
      ((< weekday first-weekday)
       ;; The first day is next week.
       (+ 8 (- weekday first-weekday)))
      (else
       (+ 1 (- weekday first-weekday))))))

(define (last-in-month year month weekday)
  (let* ((last-day (days-in--month year month))
         (day-of-week (date-iso-weekday (make-date year month last-day))))
    (if (< weekday day-of-week)
        (- last-day (- day-of-week weekday))
        (+ (- last-day 7)
           (- weekday day-of-week)))))

(define (nth-in-month year month weekday number-of-weeks)
  (let* ((day (first-in-month year month weekday))
         (day (+ day (* 7 (- number-of-weeks 1)))))
    (and (<= day (days-in-month year month))
         day)))
