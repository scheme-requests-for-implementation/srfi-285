;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define (unix-timestamp->ntp-timestamp unix)
  (+ unix 2208988800))

(define (ntp-timestamp->unix-timestamp ntp)
  (- ntp 2208988800))