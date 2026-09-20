;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (srfi 285 internal timestamp)
  (import (scheme base)
          (scheme case-lambda)
          (srfi 285 date)
          (srfi 285 clock-time)
          (srfi 285 internal database)
          (srfi 285 internal time-delta)
          (srfi 285 internal util)
          (srfi 285 moment)
          (tzif-parser local-time))
  (export timestamp? timestamp-date timestamp-clock-time
          timestamp-timezone timestamp-fold
          timestamp-ymd timestamp-year timestamp-month timestamp-day
          timestamp-hms timestamp-hour timestamp-minute timestamp-second
          timestamp-timezone-abbreviation timestamp-timezone-dst?
          make-timestamp date+clock-time->timestamp
          moment->timestamp timestamp->moment
          timestamp-in-timezone
          timestamp-timezone-offset
          timestamp+
          timestamp->iso-8601
          posix-time->utc-timestamp
          timestamp->utc-posix-time)
  (include "timestamp.scm"))