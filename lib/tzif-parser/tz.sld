;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (tzif-parser tz)
  (import (scheme base))
  (export tz-offset tz-offset?
          tz-offset-sign-negative?
          tz-offset-hour
          tz-offset-minute
          tz-offset-second
          tz-month-week-day tz-month-week-day?
          tz-month-week-day:month
          tz-month-week-day:week
          tz-month-week-day:day
          tz-julian-without-leap-day
          tz-julian-without-leap-day?
          tz-julian-without-leap-day:date
          tz-julian-with-leap-day
          tz-julian-with-leap-day?
          tz-julian-with-leap-day:date
          tz-transition tz-transition?
          tz-transition-date tz-transition-time
          tz-string tz-string?
          tz-string-stdname tz-string-stdoffset
          tz-string-dstname tz-string-dstoffset
          tz-string-change-to-dst
          tz-string-change-to-std
          tz-string->datum

          parse-tz)
  (include "tz.scm"))


