;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (srfi NNN date)
  (import (scheme base)
          (srfi NNN internal date))
  (export make-date date? date-ymd
          date-year date-month date-day
          date->iso-8601
          date-iso-week date-iso-week-year
          date-iso-weekday
          date->mjd mjd->date
          date->rata-die rata-die->date
          date=? date<? date<=? date>? date>=?)
  (include "date.scm"))