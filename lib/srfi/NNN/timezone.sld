;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (srfi NNN timezone)
  (import (srfi NNN internal database))
  (export timezone? utc-timezone utc-offset-timezone
          timezone-earliest timezone-latest
          system-timezone))