;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (srfi NNN gregorian)
  (import (scheme base)
          (srfi NNN date)
          (srfi NNN internal date))
  (export days-in-month
          day-name->weekday
          first-in-month last-in-month nth-in-month
          leap-year?)
  (include "gregorian.scm"))