;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (tzif-parser utils)
  (import (scheme base))
  (export unix-timestamp->ntp-timestamp ntp-timestamp->unix-timestamp)
  (include "utils.scm"))