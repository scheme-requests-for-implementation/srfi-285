;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (srfi 285 timestamp)
  (import (srfi 285 internal timestamp))
  (export make-timestamp date+clock-time->timestamp
          timestamp? moment->timestamp timestamp->moment
          timestamp-in-timezone
          posix-time->utc-timestamp timestamp->utc-posix-time
          timestamp-date timestamp-ymd
          timestamp-year timestamp-month timestamp-day
          timestamp-clock-time timestamp-hms
          timestamp-hour timestamp-minute timestamp-second
          timestamp-timezone
          timestamp-fold
          timestamp-timezone-offset
          timestamp-timezone-abbreviation timestamp-timezone-dst?
          timestamp->iso-8601))