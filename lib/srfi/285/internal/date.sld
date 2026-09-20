;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (srfi 285 internal date)
  (import (scheme base))
  (export days-in-month leap-year?)
  (include "date.scm"))