;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (srfi NNN internal dst)
  (import (scheme base) (scheme write)
          (srfi NNN date)
          (srfi NNN gregorian)
          (srfi NNN internal time-delta)
          (tzif-parser tz)
          (tzif-parser local-time))
  (export transition-unix transition-local
          add-offset
          in-offset offset-rule find-an-offset-rule)
  (include "dst.scm"))