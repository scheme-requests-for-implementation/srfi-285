;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (srfi 285 gregorian)
  (import (scheme base)
          (srfi 285 date)
          (srfi 285 internal date))
  (export days-in-month
          day-name->weekday
          first-in-month last-in-month nth-in-month
          leap-year?)
  (include "gregorian.scm"))