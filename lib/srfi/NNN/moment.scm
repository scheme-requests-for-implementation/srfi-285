;;; SPDX-FileCopyrightText: 2024-2026 Arvydas Silanskas
;;; SPDX-License-Identifier: MIT

(define-record-type <moment>
  (make-moment* date second)
  moment?
  (date moment-date)
  (second moment-second-of-day))

(define (make-moment date second)
  (unless (and (date? date)
               (real? second)
               (exact? second)
               (<= 0 second)
               (< second 86400))
    (error "make-moment called with invalid parameters" date second))
  (make-moment* date second))

(define (make-transitive binary-predicate)
  (lambda (x y . rest)
    (let loop ((x x) (y y) (rest rest))
      (and (binary-predicate x y)
           (or (null? rest)
               (loop y (car rest) (cdr rest)))))))

(define (%moment=? m1 m2)
  (unless (and (moment? m1) (moment? m2))
    (error "moment=? called with invalid parameters" m1 m2))
  (and (date=? (moment-date m1) (moment-date m2))
       (= (moment-second-of-day m1) (moment-second-of-day m2))))
(define moment=? (make-transitive %moment=?))

(define (%moment<? m1 m2)
  (unless (and (moment? m1) (moment? m2))
    (error "moment<? called with invalid parameters" m1 m2))
  (or (date<? (moment-date m1) (moment-date m2))
      (and (date=? (moment-date m1) (moment-date m2))
           (< (moment-second-of-day m1) (moment-second-of-day m2)))))
(define moment<? (make-transitive %moment<?))

(define (%moment<=? m1 m2)
  (unless (and (moment? m1) (moment? m2))
    (error "moment<=? called with invalid parameters" m1 m2))
  (or (date<? (moment-date m1) (moment-date m2))
      (and (date=? (moment-date m1) (moment-date m2))
           (<= (moment-second-of-day m1) (moment-second-of-day m2)))))
(define moment<=? (make-transitive %moment<=?))

(define (%moment>? m1 m2)
  (unless (and (moment? m1) (moment? m2))
    (error "moment>? called with invalid parameters" m1 m2))
  (or (date>? (moment-date m1) (moment-date m2))
      (and (date=? (moment-date m1) (moment-date m2))
           (> (moment-second-of-day m1) (moment-second-of-day m2)))))
(define moment>? (make-transitive %moment>?))

(define (%moment>=? m1 m2)
  (unless (and (moment? m1) (moment? m2))
    (error "moment>=? called with invalid parameters" m1 m2))
  (or (date>? (moment-date m1) (moment-date m2))
      (and (date=? (moment-date m1) (moment-date m2))
           (>= (moment-second-of-day m1) (moment-second-of-day m2)))))
(define moment>=? (make-transitive %moment>=?))
