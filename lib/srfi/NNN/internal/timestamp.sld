;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (srfi NNN internal timestamp)
  (import (scheme base)
          (scheme case-lambda)
          (srfi NNN date)
          (srfi NNN clock-time)
          (srfi NNN internal database)
          (srfi NNN internal time-delta)
          (srfi NNN internal util)
          (srfi NNN moment)
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