;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (srfi NNN internal database)
  (import (scheme base) (scheme write)
          (srfi NNN date)
          (srfi NNN clock-time)
          (srfi NNN internal time-delta)
          (srfi NNN internal dst)
          (srfi NNN internal util)
          (srfi NNN moment)
          (tzif-parser tz)
          (tzif-parser modify-leap-seconds)
          (tzif-parser discover-tzdb)
          (tzif-parser tzif)
          (tzif-parser leap-seconds)
          (tzif-parser local-time)
          (tzif-parser utils))
  (export timezones timezone-names tz-timezone
          timezone-earliest timezone-latest
          leapsecond-info/tai leapsecond-info/utc
          reload-timezone-data!
          tzrule tzrule?
          tzrule-std-name tzrule-std-offset tzrule-dst-transition
          tzrule-dst-name tzrule-dst-offset tzrule-std-transition
          make-timezone timezone?
          timezone-first timezone-rule
          timezone-utc-ranges timezone-local-ranges
          find-offset/wall find-offset/utc
          utc-timezone utc-offset-timezone system-timezone)
  (include "database.scm"))