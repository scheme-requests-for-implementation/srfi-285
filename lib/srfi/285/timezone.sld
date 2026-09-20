;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (srfi 285 timezone)
  (import (srfi 285 internal database))
  (export timezone? utc-timezone utc-offset-timezone
          timezone-earliest timezone-latest
          system-timezone))