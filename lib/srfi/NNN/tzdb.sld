;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (srfi NNN tzdb)
  (import (scheme base) (srfi NNN internal database))
  (export reload-timezone-data!
          tz-timezone tz-timezones)
  (begin
    (define (tz-timezones) timezone-names)))