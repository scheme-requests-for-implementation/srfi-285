;;; SPDX-FileCopyrightText: 2024-2026 Arvydas Silanskas
;;; SPDX-License-Identifier: MIT

(define (days-in-month is-leap-year month succ fail)
  (case month
      ((1 3 5 7 8 10 12) (succ 31))
      ((4 6 9 11)        (succ 30))
      ((2)               (succ (if is-leap-year 29 28)))
      (else (fail))))

(define (leap-year? year)
  (cond
    ((not (zero? (floor-remainder year 4))) #f)
    ((not (zero? (floor-remainder year 100))) #t)
    (else (zero? (floor-remainder year 400)))))

