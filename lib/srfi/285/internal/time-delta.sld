;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (srfi 285 internal time-delta)
  (import (scheme base) (srfi 1)
          (srfi 285 date)
          (srfi 285 internal date))
  (export time-delta?
          time-delta-years time-delta-months time-delta-weeks
          time-delta-days
          time-delta-hours time-delta-minutes time-delta-seconds
          years-delta months-delta weeks-delta days-delta
          hours-delta minutes-delta seconds-delta
          time-delta+ time-delta-negate
          date+)
  (include "time-delta.scm"))