;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (srfi 285 internal dst)
  (import (scheme base) (scheme write)
          (srfi 285 date)
          (srfi 285 gregorian)
          (srfi 285 internal time-delta)
          (tzif-parser tz)
          (tzif-parser local-time))
  (export transition-unix transition-local
          add-offset
          in-offset offset-rule find-an-offset-rule)
  (include "dst.scm"))