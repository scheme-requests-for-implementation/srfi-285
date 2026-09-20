;;; FIXME: Cache values of timezone-offset, etc.
;;; SPDX-FileCopyrightText: 2024-2026 Arvydas Silanskas
;;; SPDX-License-Identifier: MIT
;;; SPDX-FileCopyrightText: 2026 Peter McGoron

(define-record-type <timestamp>
  (make-timestamp* date time timezone fold)
  timestamp?
  (date timestamp-date)
  (time timestamp-clock-time)
  (timezone timestamp-timezone)
  (fold timestamp-fold))

(define (timestamp-ymd timestamp)
    (unless (timestamp? timestamp)
        (error "timestamp-ymd called with invalid parameters" timestamp))
    (date-ymd (timestamp-date timestamp)))

(define (timestamp-year timestamp)
    (unless (timestamp? timestamp)
        (error "timestamp-year called with invalid parameters" timestamp))
    (date-year (timestamp-date timestamp)))

(define (timestamp-month timestamp)
    (unless (timestamp? timestamp)
        (error "timestamp-month called with invalid parameters" timestamp))
    (date-month (timestamp-date timestamp)))

(define (timestamp-day timestamp)
    (unless (timestamp? timestamp)
        (error "timestamp-day called with invalid parameters" timestamp))
    (date-day (timestamp-date timestamp)))

(define (timestamp-hms timestamp)
    (unless (timestamp? timestamp)
        (error "timestamp-hms called with invalid parameters" timestamp))
    (clock-time-hms (timestamp-clock-time timestamp)))

(define (timestamp-hour timestamp)
    (unless (timestamp? timestamp)
        (error "timestamp-hour called with invalid parameters" timestamp))
    (clock-time-hour (timestamp-clock-time timestamp)))

(define (timestamp-minute timestamp)
    (unless (timestamp? timestamp)
        (error "timestamp-minute called with invalid parameters" timestamp))
    (clock-time-minute (timestamp-clock-time timestamp)))

(define (timestamp-second timestamp)
    (unless (timestamp? timestamp)
        (error "timestamp-second called with invalid parameters" timestamp))
    (clock-time-second (timestamp-clock-time timestamp)))

(define make-timestamp
  (case-lambda
    ((year month day hour minute second timezone)
     (date+clock-time->timestamp (make-date year month day)
                                 (make-clock-time hour minute second)
                                 timezone
                                 0))
    ((year month day hour minute second timezone fold)
     (date+clock-time->timestamp (make-date year month day)
                                 (make-clock-time hour minute second)
                                 timezone
                                 fold))))

(define unix-epoch-rd (date->rata-die (make-date 1970 1 1)))

;; returns count of seconds (excluding leap) since 1970-01-01 in current timezone
;; used to find offset from timezone rules vector
(define (timestamp->local-timepoint timestamp)
    (define days (- (date->rata-die (timestamp-date timestamp))
                    unix-epoch-rd))
    (define-values (h m s) (timestamp-hms timestamp))
    (+ (* days 86400) (* h 3600) (* m 60) s))

;; not all hours / minutes are valid during transition between DST
;; if fold is 1, also tests if there is possible time overlap
(define (validate-timestamp-time-in-timezone timestamp
                                             ok
                                             err-bad-time
                                             err-bad-fold)
    (let* ((local-timepoint (timestamp->local-timepoint timestamp))
           (tz (timestamp-timezone timestamp))
           (offset (find-offset/wall tz local-timepoint))
           (fold (timestamp-fold timestamp)))
      (cond
        ((= 0 (vector-length offset)) (err-bad-time))
        ((and (= 1 fold) (< (vector-length offset) 2)) (err-bad-fold))
        (else (ok)))))

(define (validate-timestamp-leapsecond timestamp ok err)
  (if (< (timestamp-second timestamp) 60)
      (ok)
      (let* ((timestamp (timestamp-in-utc timestamp))
             (timepoint (floor (timestamp->local-timepoint timestamp))))
        (define-values (leap? offset) (leapsecond-info/utc timepoint))
        (if leap?
            (ok)
            (err)))))

(define date+clock-time->timestamp
  (case-lambda
    ((date time timezone)
     (date+clock-time->timestamp date time timezone 0))
    ((date time timezone fold)
     (unless (and (date? date)
                  (clock-time? time)
                  (timezone? timezone)
                  (number? fold)
                  (or (= 0 fold) (= 1 fold)))
       (error "date+clock-time->timestamp called with invalid parameters"
              date time timezone fold))
     (let ((timestamp (make-timestamp* date time timezone fold)))
       (validate-timestamp-time-in-timezone
        timestamp
        (lambda _ #t)
        (lambda _ (error "date+clock-time->timestamp called with invalid \
local time for the given timezone"
                         date time timezone fold))
        (lambda _ (error "date+clock-time->timestamp called with invalid \
fold value for given local time in the given timezone"
                         date time timezone fold)))
       (validate-timestamp-leapsecond
        timestamp
        (lambda _ #t)
        (lambda _ (error "date+clock-time->timestamp called with invalid \
(leap) second value"
                         date
                         time
                         timezone
                         fold)))
       timestamp))))

(define (moment->timestamp moment tz)
    (unless (and (moment? moment)
                 (timezone? tz))
        (error "moment->timestamp called with invalid parameters" moment tz))
    (let* ((date (moment-date moment))
           (seconds (moment-second-of-day moment))
           (days (- (date->rata-die date) unix-epoch-rd))
           (timepoint (+ (* days 86400) seconds))
           (leap?+offset (call-with-values
                             (lambda () (leapsecond-info/tai timepoint))
                             cons))
           (leap? (car leap?+offset))
           (offset (cdr leap?+offset))
           (seconds (- seconds offset))
           (seconds-full (floor seconds))
           (seconds-frac (- seconds seconds-full))
           (seconds seconds-full)
           (date-diff (floor-quotient seconds 86400))
           (date (if (= date-diff 0)
                     date
                     (date+ date (days-delta date-diff))))
           (seconds (floor-remainder seconds 86400))
           (hours (floor-quotient seconds 3600))
           (seconds (floor-remainder seconds 3600))
           (minutes (floor-quotient seconds 60))
           (seconds (floor-remainder seconds 60))
           (seconds (+ seconds seconds-frac))
           (seconds (if leap?
                        (+ 1 seconds)
                        seconds))
           (timestamp (date+clock-time->timestamp date
                                                  (make-clock-time hours
                                                                   minutes
                                                                   seconds)
                                                  (utc-timezone))))
      (timestamp-in-timezone timestamp tz)))

(define (timestamp-in-utc timestamp)
    (let* ((dt (timestamp-timezone-offset timestamp))
           (dt (time-delta-negate dt))
           (timestamp (timestamp+/no-rules timestamp dt)))
      (make-timestamp* (timestamp-date timestamp)
                       (timestamp-clock-time timestamp)
                       (utc-timezone)
                       0)))


(define (time-delta->seconds time-delta)
  (+ (* (time-delta-days time-delta) 60 60 24)
     (* (time-delta-hours time-delta) 60 60)
     (time-delta-seconds time-delta)))

(define (timestamp-in-timezone timestamp tz)
  (define tz* (timestamp-timezone timestamp))
  (cond
    ((eqv? tz tz*) timestamp)
    (else
     (let* ((timestamp (timestamp-in-utc timestamp))
            (offsets-vec (find-offset/utc
                          tz
                          (timestamp->local-timepoint timestamp)))
            (offset (time-range-offset (vector-ref offsets-vec 0)))
            (timestamp (timestamp+/no-rules timestamp offset))
            (timestamp (date+clock-time->timestamp
                        (timestamp-date timestamp)
                        (timestamp-clock-time timestamp)
                        tz))
            ;; recompute offsets-vec from wall time; if the size > 1, need to set proper fold value
            (offsets-vec (find-offset/wall
                          tz
                          (timestamp->local-timepoint timestamp)))
            (fold (and (> (vector-length offsets-vec) 1)
                       (= (time-delta->seconds offset)
                          (time-delta->seconds
                           (time-range-offset
                            (vector-ref offsets-vec 1)))))))
       (if fold
           (date+clock-time->timestamp (timestamp-date timestamp)
                                       (timestamp-clock-time timestamp)
                                       tz
                                       1)
           timestamp)))))

(define (timestamp-timezone-offset timestamp)
    (unless (timestamp? timestamp)
        (error "timestamp-timezone-offset called with invalid parameters" timestamp))
    (let* ((local-timepoint (timestamp->local-timepoint timestamp))
           (tz (timestamp-timezone timestamp))
           (fold (timestamp-fold timestamp))
           (offsets-vec (find-offset/wall tz local-timepoint)))
      (time-range-offset (vector-ref offsets-vec fold))))

(define (timestamp-timezone-abbreviation timestamp)
  (unless (timestamp? timestamp)
    (error "not a timestamp" timestamp))
  (let* ((local-timepoint (timestamp->local-timepoint timestamp))
         (tz (timestamp-timezone timestamp))
         (fold (timestamp-fold timestamp))
         (offsets-vec (find-offset/wall tz local-timepoint)))
    (time-range-designation (vector-ref offsets-vec fold))))

(define (timestamp-timezone-dst? timestamp)
  (unless (timestamp? timestamp)
    (error "not a timestamp" timestamp))
  (let* ((local-timepoint (timestamp->local-timepoint timestamp))
         (tz (timestamp-timezone timestamp))
         (fold (timestamp-fold timestamp))
         (offsets-vec (find-offset/wall tz local-timepoint)))
    (time-range-dst? (vector-ref offsets-vec fold))))

(define (timestamp->iso-8601 timestamp)
    (define (format-second s)
        (let* ((full (round s))
               (full-str (left-pad (number->string full) #\0 2))
               (frac (- s full))
               (has-frac (not (= 0 frac))))
          (string-append
              full-str
              (if has-frac "." "")
              (if has-frac (format-second-frac frac) ""))))
    (define (format-second-frac n)
        (do ((digits (round (* 1000000000 n)) (/ digits 10)))
            ((> (floor-remainder digits 10) 0) (number->string digits))))
    (define (format-tz dt)
        (unless (= 0 (time-delta-seconds dt))
            (error "timestamp->iso-8601 called with timezone that has second offset" timestamp))
        (if (and (= 0 (time-delta-hours dt))
                 (= 0 (time-delta-minutes dt)))
            "Z"
            (let ((sign (if (< (time-delta-hours dt) 0) "-" "+")))
                        (string-append
                            sign
                            (left-pad (number->string (abs (time-delta-hours dt))) #\0 2)
                            ":"
                            (left-pad (number->string (abs (time-delta-minutes dt))) #\0 2)))))
    (unless (timestamp? timestamp)
        (error "timestamp->iso-8601 called with invalid parameters" timestamp))
    (string-append
        (date->iso-8601 (timestamp-date timestamp))
        "T"
        (left-pad (number->string (timestamp-hour timestamp)) #\0 2)
        ":"
        (left-pad (number->string (timestamp-minute timestamp)) #\0 2)
        ":"
        (format-second (timestamp-second timestamp))
        (format-tz (timestamp-timezone-offset timestamp))))

(define (timestamp+ timestamp dt)
  (unless (and (timestamp? timestamp)
               (time-delta? dt))
    (date-error "timestamp+ called with invalid parameters" timestamp dt))
  (let* ((tz (timestamp-timezone timestamp))
         (timestamp (timestamp-in-utc timestamp))
         (date (timestamp-date timestamp))
         (clock (timestamp-clock-time timestamp))
         ;; reuse date+ for adding years/months/weeks/days
         (date-dt (time-delta+ (years-delta (time-delta-years dt))
                               (months-delta (time-delta-months dt))
                               (weeks-delta (time-delta-weeks dt))
                               (days-delta (time-delta-days dt))))
         (date (date+ date date-dt))
         ;; add minutes and hours together because they're consistently of same size
         (old-minutes (+ (* 60 (clock-time-hour clock)) (clock-time-minute clock)))
         (minutes-diff (+ (* 60 (time-delta-hours dt)) (time-delta-minutes dt)))
         (minutes (+ old-minutes minutes-diff))
         (days-diff (floor-quotient minutes (* 24 60)))
         (date (date+ date (days-delta days-diff)))
         (minutes (floor-remainder minutes (* 24 60)))
         (hours (floor-quotient minutes 60))
         (minutes (floor-remainder minutes 60))
         (clock (make-clock-time hours minutes (clock-time-second clock)))
         ;; need to check the seconds value is valid in new timestamp, in case initial timestamp was on the leap second
         (timestamp (let* ((t (make-timestamp* date clock (timestamp-timezone timestamp) 0))
                           (valid? (validate-timestamp-leapsecond t (lambda _ #t) (lambda _ #f))))
                      (if valid?
                          t
                          (make-timestamp* date
                                           (make-clock-time hours minutes (- (clock-time-second clock) 1))
                                           (timestamp-timezone timestamp)
                                           0))))
         ;; seconds component left. If non zero, do calculation in TAI and convert back to handle potential leaps
         (timestamp (if (= 0 (time-delta-seconds dt))
                        timestamp
                        (moment->timestamp (moment+seconds (timestamp->moment timestamp) (time-delta-seconds dt))
                                           (utc-timezone)
                                           ))))
    (timestamp-in-timezone timestamp tz)))

(define (moment+seconds moment seconds+)
    (let* ((date (moment-date moment))
           (second (moment-second-of-day moment))
           (second (+ second seconds+))
           (second-full (floor second))
           (second-frac (- second second-full))
           (second second-full)
           (days+ (floor-quotient second 86400))
           (second (floor-remainder second 86400))
           (second (+ second second-frac))
           (date (if (= 0 days+)
                     date
                     (date+ date (days-delta days+)))))
      (make-moment date second)))

;; timestamp+ but ignoring timestamp's timezone rules and leap seconds
;; used to apply timezone dt
(define (timestamp+/no-rules timestamp dt)
    (let* ((leap? (>= (timestamp-second timestamp) 60))
           (date (timestamp-date timestamp))
           (seconds (+ (* 3600 (+ (timestamp-hour timestamp) (time-delta-hours dt)))
                       (* 60 (+ (timestamp-minute timestamp) (time-delta-minutes dt)))
                       (timestamp-second timestamp)
                       (time-delta-seconds dt)))
           (seconds (if leap?
                        (- seconds 1)
                        seconds))
           (seconds-full (floor seconds))
           (seconds-frac (- seconds seconds-full))
           (seconds seconds-full)
           (days-diff (floor-quotient seconds 86400))
           (date (if (= days-diff 0)
                     date
                     (date+ date (days-delta days-diff))))
           (seconds (floor-remainder seconds 86400))
           (hours (floor-quotient seconds 3600))
           (seconds (floor-remainder seconds 3600))
           (minutes (floor-quotient seconds 60))
           (seconds (floor-remainder seconds 60))
           (seconds (+ seconds seconds-frac))
           (seconds (if leap?
                        (+ seconds 1)
                        seconds))
           (clock (make-clock-time hours minutes seconds)))
      (make-timestamp* date clock (timestamp-timezone timestamp) 0)))

(define (timestamp->utc-posix-time timestamp)
    (define timestamp* (timestamp-in-timezone timestamp (utc-timezone)))
    (define leap? (>= (timestamp-second timestamp*) 60))
    (define timepoint (timestamp->local-timepoint timestamp*))
    (if leap?
        (- timepoint 1)
        timepoint))

(define (posix-time->utc-timestamp* seconds)
    (let* ((days (floor-quotient seconds 86400))
           (date (rata-die->date (+ unix-epoch-rd days)))
           (seconds (floor-remainder seconds 86400))
           (hours (floor-quotient seconds 3600))
           (seconds (floor-remainder seconds 3600))
           (minutes (floor-quotient seconds 60))
           (seconds (floor-remainder seconds 60))
           (clock (make-clock-time hours minutes seconds)))
      (date+clock-time->timestamp date clock (utc-timezone))))

(define posix-time->utc-timestamp
  (case-lambda
    ((seconds)
     (unless (and (exact? seconds)
                  (rational? seconds))
       (error "posix-time->utc-timestamp called with invalid \
parameters" seconds))
     (posix-time->utc-timestamp (exact (truncate seconds))
                                (exact
                                 (round
                                  (* #e1e9
                                     (- seconds (truncate seconds)))))))
    ((seconds nano-seconds)
     (unless (and (integer? seconds)
                  (integer? nano-seconds))
       (error "posix-time->utc-timestamp called with \
invalid parameters" seconds nano-seconds))
     (let* ((days (floor-quotient seconds 86400))
            (date (rata-die->date (+ unix-epoch-rd days)))
            (seconds (floor-remainder seconds 86400))
            (hours (floor-quotient seconds 3600))
            (seconds (floor-remainder seconds 3600))
            (minutes (floor-quotient seconds 60))
            (seconds (floor-remainder seconds 60))
            (seconds (+ seconds (/ nano-seconds #e1e9)))
            (clock (make-clock-time hours minutes seconds)))
       (date+clock-time->timestamp date clock (utc-timezone))))))

(define (timestamp->moment timestamp)
  (unless (timestamp? timestamp)
    (data-error "timestamp->moment called with invalid parameters" timestamp))
  (let* ((timestamp (timestamp-in-utc timestamp))
         (timepoint (floor (timestamp->local-timepoint timestamp)))
         (offset (call-with-values
                     (lambda () (leapsecond-info/utc timepoint))
                   (lambda (leap? offset) offset)))
         (seconds (+ (* (timestamp-hour timestamp) 3600)
                     (* (timestamp-minute timestamp) 60)
                     (timestamp-second timestamp) offset))
         (seconds-full (floor seconds))
         (seconds-frac (- seconds seconds-full))
         (seconds seconds-full)
         (date (timestamp-date timestamp))
         (date-diff (floor-quotient seconds 86400))
         (seconds (floor-remainder seconds 86400))
         (seconds (+ seconds seconds-frac))
         (date (if (= date-diff 0)
                   date
                   (date+ date (days-delta date-diff)))))
    (make-moment date seconds)))
