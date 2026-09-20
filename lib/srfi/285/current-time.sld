;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (srfi 285 current-time)
  (import (scheme base)
          (srfi 285 timestamp)
          (srfi 285 timezone))
  (export current-moment
          current-utc-timestamp
          current-system-timestamp)
  (cond-expand
    (gauche
     (import (gauche base))
     (begin
       (define (current-moment)
         (let-values (((seconds microseconds)
                       (sys-gettimeofday)))
           (timestamp->moment
            (posix-time->utc-timestamp seconds
                                       (* microseconds 1000)))))))
    (else
     (import (scheme time))
     ;; FIXME
     (begin
       (define (current-moment)
         (let ((s (current-second)))
           (timestamp->moment
            (posix-time->utc-timestamp (exact s))))))))
  (begin
    (define (current-utc-timestamp)
      (moment->timestamp (current-moment) (utc-timezone)))
    (define (current-system-timestamp)
      (moment->timestamp (current-moment) (system-timezone)))))