;;; SPDX-FileCopyrightText: 2024-2026 Arvydas Silanskas
;;; SPDX-License-Identifier: MIT
;;; SPDX-FileCopyrightText: 2026 Peter McGoron

(define-library (srfi NNN internal util)
  (import (scheme base))
  (export left-pad)
  (begin
    (define (left-pad str c min-length)
      (define pad-size (max 0 (- min-length (string-length str))))
      (if (> pad-size 0)
          (let ((pad (make-string pad-size c)))
            (string-append pad str))
          str))))