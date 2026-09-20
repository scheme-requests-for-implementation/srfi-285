;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (tzif-parser tzif)
  (import (scheme base) (tzif-parser tz) (scheme file)
          (scheme write))
  (cond-expand
    ((library (srfi 281)) (import (srfi 281)))
    (else))
  (export read-tzif-simple tzif-file?
          assert-magic-string!
          read-version
          tzif-header tzif-header?
          tzif-header-version tzif-header-isutcnt
          tzif-header-isstdcnt tzif-header-leapcnt
          tzif-header-timecnt tzif-header-charcnt
          tzif-header->datum
          read-header

          tzif-leap-second tzif-leap-second?
          tzif-leap-second-occurence tzif-leap-second-correction
          tzif-leap-second->datum

          tzif-leap-seconds-record tzif-leap-seconds-record?
          tzif-leap-seconds-record-vector
          tzif-leap-seconds-record-expiry
          tzif-leap-seconds-record->datum

          tzif-transition tzif-transition?
          tzif-transition-time tzif-transition-offset
          tzif-transition-dst? tzif-transition-designation
          tzif-transition-standard? tzif-transition-universal-time?
          tzif-transition->datum

          tzif-transition-local-time-unspecified?

          read-block
          read-footer)
  (include "tzif.scm"))