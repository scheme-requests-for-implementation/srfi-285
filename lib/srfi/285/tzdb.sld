;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (srfi 285 tzdb)
  (import (scheme base) (srfi 285 internal database))
  (export reload-timezone-data!
          tz-timezone tz-timezones)
  (begin
    (define (tz-timezones) timezone-names)))