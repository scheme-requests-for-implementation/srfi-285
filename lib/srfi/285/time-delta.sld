;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (srfi 285 time-delta)
  (import (srfi 285 internal time-delta)
          (srfi 285 internal timestamp))
  (export time-delta?
          time-delta-years time-delta-months time-delta-weeks
          time-delta-days
          time-delta-hours time-delta-minutes time-delta-seconds
          years-delta months-delta weeks-delta days-delta
          hours-delta minutes-delta seconds-delta
          time-delta+ time-delta-negate
          date+ timestamp+))