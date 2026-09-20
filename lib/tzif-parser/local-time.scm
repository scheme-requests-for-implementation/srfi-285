;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

;;; TZdb is in "UNIX leap time", which is basically TAI with the UNIX epoch.
;;; However, calculating what time a *timestamp*, which is a date, time, and
;;; timezone, is difficult in universal time.

(define-record-type <time-range>
  (time-range start end offset dst? designation)
  time-range?
  (start time-range-start)
  (end time-range-end)
  (offset time-range-offset)
  (dst? time-range-dst?)
  (designation time-range-designation))

(define (time-range->datum value)
  (vector 'time-range
          'start (time-range-start value)
          'end (time-range-end value)
          'offset (time-range-offset value)
          'dst? (time-range-dst? value)
          'designation (time-range-designation value)))

(define (search-ranges ranges time)
  (let loop ((l 0) (r (- (vector-length ranges) 1)))
    (if (< r l)
        #f
        (let* ((i (+ l (floor-quotient (- r l) 2)))
               (range (vector-ref ranges i))
               (start (time-range-start range))
               (end (time-range-end range)))
          (cond
            ((and (or (not start) (<= start time))
                  (or (not end) (< time end)))
             i)
            ((and start (< time start)) (loop l (- i 1)))
            (else (loop (+ i 1) r)))))))

(define (range/as-is first entries)
  ;; Entries are a vector of timestamps that are not adjusted for
  ;; their offset. These can be UTC timestamps or TAI timestamps.
  (define new-entries (make-vector (+ (vector-length entries) 1)))
  (define last (- (vector-length entries) 1))
  (vector-set! new-entries 0
               (time-range #f
                           (tzif-transition-time
                            (vector-ref entries 0))
                           (tzif-transition-offset first)
                           (tzif-transition-dst? first)
                           (tzif-transition-designation first)))
  (let ((elast (vector-ref entries last)))
    (vector-set! new-entries
                 (+ last 1)
                 (time-range (tzif-transition-time elast)
                             #f
                             (tzif-transition-offset elast)
                             (tzif-transition-dst? elast)
                             (tzif-transition-designation elast))))
  (do ((i 0 (+ i 1)))
      ((= i last) new-entries)
    (let ((here (vector-ref entries i)))
      (vector-set! new-entries
                   (+ i 1)
                   (time-range (tzif-transition-time here)
                               (tzif-transition-time
                                (vector-ref entries (+ i 1)))
                               (tzif-transition-offset here)
                               (tzif-transition-dst? here)
                               (tzif-transition-designation here))))))

(define (range/local first entries)
  ;; Entries is a vector of times with UNIX timestamps. The vectors are
  ;; converted to UNIX local timestamp seconds. These are the seconds
  ;; since Jan 1st, 1970, treating the timestamp as literal. Hence the
  ;; timestamp jumps forward entering DST, and jumps backwards exiting
  ;; from DST.
  (do ((range (range/as-is first entries))
       (i 0 (+ i 1)))
      ((= i (vector-length range)) range)
    (let* ((entry (vector-ref range i))
           (start (time-range-start entry))
           (end (time-range-end entry))
           (offset (time-range-offset entry)))
      (vector-set! range i
                   (time-range (and start (+ start offset))
                               (and end (+ end offset))
                               offset
                               (time-range-dst? entry)
                               (time-range-designation entry))))))
