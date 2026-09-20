;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (srfi NNN clock-time)
  (import (scheme base))
  (export clock-time? clock-time-hms
          make-clock-time
          clock-time-hour clock-time-minute clock-time-second)
  (include "clock-time.scm"))