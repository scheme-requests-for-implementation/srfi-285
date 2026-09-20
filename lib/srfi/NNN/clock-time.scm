;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-FileCopyrightText: 2024-2026 Arvydas Silanskas
;;; SPDX-License-Identifier: MIT

(define-record-type <clock-time>
  (make-clock-time* hour minute second)
  clock-time?
  (hour clock-time-hour)
  (minute clock-time-minute)
  (second clock-time-second))

(define (make-clock-time hour minute second)
  (unless (and (integer? hour)
               (<= 0 hour 23)
               (integer? minute)
               (<= 0 minute 59)
               (rational? second)
               (exact? second)
               (<= 0 second)
               (< second 61))
    (error "make-clock-time called with invalid parameters" hour minute second))
  (make-clock-time* hour minute second))

(define (clock-time-hms clock-time)
  (unless (clock-time? clock-time)
    (error "clock-time-hms called with invalid parameters" clock-time))
  (values (clock-time-hour clock-time)
          (clock-time-minute clock-time)
          (clock-time-second clock-time)))

