;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (srfi 285 moment)
  (import (scheme base) (srfi 285 date))
  (export make-moment moment?
          moment-date moment-second-of-day
          moment=? moment<? moment<=?
          moment>? moment>=?)
  (include "moment.scm"))