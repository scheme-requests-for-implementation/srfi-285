;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (srfi NNN internal date)
  (import (scheme base))
  (export days-in-month leap-year?)
  (include "date.scm"))