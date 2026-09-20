;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (srfi NNN moment)
  (import (scheme base) (srfi NNN date))
  (export make-moment moment?
          moment-date moment-second-of-day
          moment=? moment<? moment<=?
          moment>? moment>=?)
  (include "moment.scm"))