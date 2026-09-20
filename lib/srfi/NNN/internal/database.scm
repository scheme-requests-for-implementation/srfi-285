;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

;;; An association list from timezone names to either a filename or cached
;;; timezone data.
(define timezones #f)
(define timezone-names #f)
(define leap-seconds #f)
(define raw-leap-seconds-info #f)
(define %system-timezone #f)
(define (system-timezone) %system-timezone)

(define-record-type <tzrule>
  (tzrule std-name std-offset dst-transition
          dst-name dst-offset std-transition)
  tzrule?
  (std-name tzrule-std-name)
  (std-offset tzrule-std-offset)
  (dst-transition tzrule-dst-transition)
  (dst-name tzrule-dst-name)
  (dst-offset tzrule-dst-offset)
  (std-transition tzrule-std-transition))

(define-record-type <timezone>
  (make-timezone first utc-ranges local-ranges rule)
  timezone?
  (first timezone-first)
  (utc-ranges timezone-utc-ranges)
  (local-ranges timezone-local-ranges)
  (rule timezone-rule))

(define unix-epoch-rd
  (date->rata-die (make-date 1970 1 1)))

(define (timezone-earliest timezone)
  (let*-values (((first) (vector-ref (timezone-utc-ranges timezone) 0))
                ((utc) (time-range-end first))
                ((ignored TAI-UTC) (leapsecond-info/utc utc))
                ((days seconds) (floor/ (+ utc TAI-UTC) 86400)))
    (make-moment (rata-die->date (+ days unix-epoch-rd)) seconds)))

(define (timezone-latest timezone)
  (let*-values (((ranges) (timezone-utc-ranges timezone))
                ((range) (vector-ref ranges (- (vector-length ranges) 1)))
                ((utc) (time-range-start range))
                ((ignored TAI-UTC) (leapsecond-info/utc utc))
                ((days seconds) (floor/ (+ utc TAI-UTC) 86400)))
    (make-moment (rata-die->date (+ days unix-epoch-rd)) seconds)))

(define utc-timezone
  (let ((range (vector (time-range #f #f (seconds-delta 0) #f "UTC"))))
    (lambda ()
      (make-timezone #f range range #f))))

(define (utc-offset-timezone offset)
  (unless (time-delta? offset)
    (error "not a time delta" offset))
  (unless (and (zero? (time-delta-years offset))
               (zero? (time-delta-months offset))
               (zero? (time-delta-days offset)))
    (error "invalid time delta for UTC offset" offset))
  (let* ((sign (if (negative? (time-delta-hours offset))
                   "-"
                   ""))
         (hours (left-pad (number->string (abs (time-delta-hours offset)))
                          #\0 2))
         (minutes (abs (time-delta-minutes offset)))
         (seconds (abs (time-delta-seconds offset)))
         (str
          (cond
            ((and (zero? minutes) (zero? seconds))
             hours)
            ((zero? seconds)
             (string-append hours (left-pad (number->string minutes)
                                            #\0 2)))
            (else
             (string-append hours
                            (left-pad (number->string minutes)
                                      #\0 2)
                            (left-pad (number->string seconds)
                                      #\0 2)))))
         (range (vector (time-range #f
                                    #f
                                    offset
                                    #f
                                    (string-append sign str)))))
    (make-timezone #f range range #f)))

(define (convert-offset offset)
  ;; Convert offset to a time-delta object, if the offset exists.
  (and offset
       ;; NOTE: In TZ strings, the sign has the opposite meaning than
       ;; the usual UTC offset.
       (let ((sign (if (tz-offset-sign-negative? offset)
                       1
                       -1)))
         (time-delta+ (hours-delta (* sign (tz-offset-hour offset)))
                      (minutes-delta (* sign (tz-offset-minute offset)))
                      (seconds-delta (* sign (tz-offset-second offset)))))))

(define (convert-tzif-offsets offsets)
  (define (second-offset->time-delta off)
    (let*-values (((minutes seconds) (floor/ off 60))
                  ((hours minutes) (floor/ minutes 60)))
      (time-delta+ (hours-delta hours)
                   (minutes-delta minutes)
                   (seconds-delta seconds))))
  (vector-map (lambda (range)
                (time-range (time-range-start range)
                            (time-range-end range)
                            (second-offset->time-delta
                             (time-range-offset range))
                            (time-range-dst? range)
                            (time-range-designation range)))
              offsets))

(define (get-tzrule footer)
  (and footer
       (tzrule (tz-string-stdname footer)
               (convert-offset (tz-string-stdoffset footer))
               (tz-string-change-to-dst footer)
               (tz-string-dstname footer)
               (convert-offset (tz-string-dstoffset footer))
               (tz-string-change-to-std footer))))

(define (get-tzif filename)
  (define-values (ignored first transitions this-leap-seconds footer)
    (read-tzif-simple filename))
  (let* ((transitions-unix (remove-leap-seconds transitions
                                                (tzif-leap-seconds-record-vector
                                                 this-leap-seconds)))
         (local-ranges (range/local first transitions-unix))
         (utc-ranges (range/as-is first transitions-unix)))
    (make-timezone first
                   (convert-tzif-offsets utc-ranges)
                   (convert-tzif-offsets local-ranges)
                   (get-tzrule footer))))

(define (tz-timezone name)
  ;; Lookup the TZdb "name" in the current known timezone database.
  ;; 
  ;; The database is crawled each refresh. Once a filename has been read,
  ;; the filename is cached until the next refresh.
  (unless (string? name)
    (error "Not a string" name))
  (cond
    ((assoc name timezones)
     => (lambda (pair)
          (let ((data (cdr pair)))
            (if (string? data)
                (let ((timezone (get-tzif data)))
                  (set-cdr! pair timezone)
                  timezone)
                data))))
    (else (error "Unknown timezone name" name))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Throughout this file, a "TAI timestamp" is the number of TAI seconds
;;; such that 10 is the UNIX epoch.
;;; 
;;; DTAI(x) = x - UTC(x) at some TAI timestamp x.
;;; 
;;; leap-seconds: A vector of <leap-second> records. Each timestamp
;;; corresponds to a POSIX timestamp where DTAI changes. Basically, one
;;; second after the leap second.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <leap-second>
  (leap-second posix delta change)
  leap-second?
  (posix leap-second-posix)
  (delta leap-second-delta)
  (change leap-second-change))
(define (ls-utc-occurence i)
  ;; Returns the UTC timestamp after the ith leap second. This is the
  ;; :00 timestamp right after the leap second, positive or negative.
  ;; 
  ;; For positive leap seconds, this timestamp could also be interpreted
  ;; as the :60 second previously.
  (leap-second-posix (vector-ref leap-seconds i)))
(define (ls-tai-occurence i)
  ;; Return the TAI timestamp after the ith leap second.
  ;; 
  ;; For example, when the leap second is positive, the returned number
  ;; is :00 in UTC, and the TAI timestamp one second before it is :60 in
  ;; UTC. When the leap second is negative, the returned number is :00
  ;; in UTC, and the TAI timestamp one second before it is :58 in UTC.
  ;; (In the negative leap second scenario, :59 does not correspond to any
  ;; TAI timestamp, or any time at all.)
  (+ (ls-utc-occurence i) (delta-tai-utc i)))
(define (delta-tai-utc i)
  ;; Return the ith change in TAI-UTC.
  (leap-second-delta (vector-ref leap-seconds i)))
(define (ls-change i)
  ;; Return the change between this TAI-UTC, and the previous one.
  (leap-second-change (vector-ref leap-seconds i)))

(define (init-leap-seconds vec)
  ;; Initialize leap seconds to be a vector of leap second occurences.
  ;; This removes some values in the leap-seconds.list list that only
  ;; describe the initial value of delta-tai.
  (define lst '())
  (let loop ((i 0) (current-delta 10))
    (if (= i (vector-length vec))
        (list->vector (reverse lst))
        (let* ((data (vector-ref vec i))
               (time (leap-second-occurence-time data))
               (delta (leap-second-occurence-delta-tai data)))
          (unless (= delta current-delta)
            ;; Only add updates to leap seconds.
            (set! lst (cons (leap-second (ntp-timestamp->unix-timestamp
                                          time)
                                         delta
                                         (- delta current-delta))
                            lst)))
          (loop (+ i 1) delta)))))

;;; ;;;;;;;;;;;
;;; Expiry is a TAI timestamp that describes when the data in the
;;; TAI table is no longer valid. It is #f if there is no expiry for
;;; the leap second table.
;;; ;;;;;;;;;;;

(define leap-seconds-expiry #f)

(define (set-leap-seconds-expiry! info)
  ;; The leap-seconds vector needs to be initialized first.
  ;; 
  ;; The expiry is an NTP timestamp. So convert it using the last
  ;; delta-tai.
  (let ((expiry (leap-seconds-info-expiry info)))
    (if (not info)
        (set! leap-seconds-expiry #f)
        (let ((delta-last (leap-second-delta
                           (vector-ref leap-seconds
                                       (- (vector-length leap-seconds)
                                          1)))))
          (set! leap-seconds-expiry
                (+ (ntp-timestamp->unix-timestamp expiry)
                   delta-last))))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Procedure that reloads all active timezone data.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-leap-seconds-info)
  ;; Try all TZPATH folders until one with "leap-seconds.info" is found.
  (let ((path (discover-leap-seconds)))
    (let loop ((tries path))
      (if (null? tries)
          (error "cannot find leap seconds" path)
          (guard (exn ((read-error? exn)
                       (loop (cdr tries)))
                      (else (raise exn)))
            (read-leap-seconds (car tries)))))))

(define (find-system-timezone!)
  ;; NOTE: FreeDesktop specific
  (define (fall-back-to-UTC!)
    (display "Warning: Could not find system timezone. \
Falling back to UTC.\n" (current-error-port))
    (set! %system-timezone (utc-timezone)))
  (define (try-TZ-environment-variable!)
    (let ((env (get-environment-variable "TZ")))
      (cond
        ((and env (member env timezone-names))
         (set! %system-timezone (tz-timezone env)))
        (else (fall-back-to-UTC!)))))
  (define (try-/etc/localtime!)
    (guard (x (else (try-TZ-environment-variable!)))
      (set! %system-timezone (get-tzif "/etc/localtime"))))
  (try-/etc/localtime!))

(define (reload-timezone-data!)
  (define leap-seconds-info (read-leap-seconds-info))
  (set! raw-leap-seconds-info leap-seconds-info)
  (set! timezones (discover-tzpath))
  (set! timezone-names (map car timezones))
  (set! leap-seconds (init-leap-seconds (leap-seconds-info-occurences
                                         leap-seconds-info)))
  (set-leap-seconds-expiry! leap-seconds-info)
  (find-system-timezone!))

;;; ;;;;;;;;;;;;;;;;;
;;; Query leap seconds information in different timestamps.
;;; ;;;;;;;;;;;;;;;;;

(define (leapsecond-info/tai tai-timestamp)
  ;; Returns two values:
  ;; 
  ;; 1. A boolean to determine if this TAI timestamp represents a leap
  ;;    second. That is, it is a TAI timestamp that has no unique POSIX
  ;;    timestamp equivalent.
  ;; 2. TAI - UTC at this point in time.
  ;; 
  ;; If the timestamp is at the leap second, then the offset is the
  ;; offset after that timestamp.
  (define (timestamp-on-leap-second? i)
    (and (positive? (ls-change i))
         (= (truncate tai-timestamp)
            (- (ls-tai-occurence i) (ls-change i)))))
  (define (before-leap-second? i)
    (if (positive? (ls-change i))
        (< tai-timestamp (- (ls-tai-occurence i) (ls-change i)))
        (< tai-timestamp (ls-tai-occurence i))))
  (if (before-leap-second? 0)
      (values #f 10)
      (let loop ((i 0))
        (cond
          ((= i (- (vector-length leap-seconds) 1))
           (values (timestamp-on-leap-second? i)
                   (delta-tai-utc i)))
          ((timestamp-on-leap-second? i)
           (values #t (delta-tai-utc i)))
          ((and (<= (ls-tai-occurence i) tai-timestamp)
                (before-leap-second? (+ i 1)))
           (values #f (delta-tai-utc i)))
          (else (loop (+ i 1)))))))

(define (leapsecond-info/utc utc-timestamp)
  ;; Returns two values:
  ;; 
  ;; 1. A boolean to determine if this UTC timestamp *might* be a leap
  ;; second. That is, it could be :60 or :00. For negative leap seconds,
  ;; this is never true.
  ;; 
  ;; 2. TAI-UTC at this point in time.
  ;; 
  ;; If the timestamp might be a positive leap second, the returned delta
  ;; is the previous delta. The most natural interpretation of the two
  ;; values is that the seconds timestamp is :60.
  (define (timestamp-on-leap-second? i)
    (and (positive? (ls-change i))
         (= (truncate utc-timestamp) (ls-utc-occurence i))))
  (define (before-or-on-leap-second? i)
    (or (< utc-timestamp (ls-utc-occurence i))
        (timestamp-on-leap-second? i)))
  (cond
    ((< utc-timestamp (ls-utc-occurence 0))
     (values #f 10))
    ((timestamp-on-leap-second? 0)
     (values #t 10))
    (else
     (let loop ((i 0))
       (cond
         ((= i (- (vector-length leap-seconds) 1))
          (values (timestamp-on-leap-second? i)
                  (delta-tai-utc i)))
         ((and (< (ls-utc-occurence i) utc-timestamp)
               (before-or-on-leap-second? (+ i 1)))
          (values (timestamp-on-leap-second? (+ i 1))
                  (delta-tai-utc i)))
         (else (loop (+ i 1))))))))

;;; ;;;;;;;;;;;;;;;;;;;;;;
;;; Query timezone information with different types of timestamps.
;;; ;;;;;;;;;;;;;;;;;;;;;;

(define (find-offset/wall timezone local-time)
  ;; Given timestamp in local UNIX seconds, find the possible ranges that
  ;; the local time is in.
  ;; 
  ;; Since offsets can go backwards in time, two ranges can overlap.
  (let* ((ranges (timezone-local-ranges timezone))
         (i (search-ranges ranges local-time))
         (is-in*
          (lambda (val)
            (let ((start (time-range-start val))
                  (end (time-range-end val)))
              (if (and (or (not start) (<= start local-time))
                       (or (not end) (< local-time end)))
                  (vector val)
                  '#()))))
         (is-in
          (lambda (i)
            (if (or (< i 0) (>= i (vector-length ranges)))
                '#()
                (is-in* (vector-ref ranges i))))))
    (cond
      ((not i) '#())
      ((and (= i (- (vector-length ranges) 1))
            (timezone-rule timezone))
       ;; Calculate rules using the POSIX timezone rules.
       ;; 
       ;; It is possible for the calculated previous transition
       ;; to overlap the second-to-last timezone entry in the table.
       ;; The RFC prohibits this, but the implementation does not do
       ;; anything to validate this.
       =>
       (lambda (rule)
         (let*-values
             (((std-offset) (tzrule-std-offset rule))
              ((dst-offset) (tzrule-dst-offset rule))
              ((std-transition) (tzrule-std-transition rule))
              ((dst-transition) (tzrule-dst-transition rule))
              ((std-designation) (tzrule-std-name rule))
              ((dst-designation) (tzrule-dst-name rule))
              ((dst->std) (lambda (year offset)
                            (add-offset
                             (transition-unix dst-offset
                                              year
                                              std-transition)
                             offset)))
              ((std->dst) (lambda (year offset)
                            (add-offset
                             (transition-unix std-offset
                                              year
                                              dst-transition)
                             offset)))
              ((year subindex) (find-an-offset-rule
                                local-time
                                dst->std std->dst
                                dst-offset std-offset))
              ((is-in)
               (lambda (subindex)
                 (offset-rule local-time
                              year subindex
                              dst->std std->dst
                              dst-offset std-offset
                              dst-designation std-designation))))
           (write (list year subindex)) (newline)
           (vector-append (is-in (- subindex 1))
                          (is-in subindex)
                          (is-in (+ subindex 1))))))
      (else
       (vector-append (is-in (- i 1))
                      (is-in i)
                      (is-in (+ i 1)))))))

(define (find-offset/utc timezone utc-time)
  ;; This is simpler, because each UTC timestamp corresponds to precisely
  ;; one range.
  (let* ((ranges (timezone-utc-ranges timezone))
         (i (search-ranges ranges utc-time)))
    (cond
      ((not i) '#())
      ((and (= i (- (vector-length ranges) 1))
            (timezone-rule timezone))
       =>
       (lambda (rule)
         (let*-values
             (((std-offset) (tzrule-std-offset rule))
              ((dst-offset) (tzrule-dst-offset rule))
              ((std-transition) (tzrule-std-transition rule))
              ((dst-transition) (tzrule-dst-transition rule))
              ((std-designation) (tzrule-std-name rule))
              ((dst-designation) (tzrule-dst-name rule))
              ((dst->std) (lambda (year)
                            (transition-unix dst-offset
                                             year
                                             std-transition)))
              ((std->dst) (lambda (year)
                            (transition-unix std-offset
                                             year
                                             dst-transition)))
              ((year subindex) (find-an-offset-rule
                                utc-time
                                dst->std std->dst)))
           (vector (offset-rule utc-time year subindex
                                dst->std std->dst
                                dst-offset std-offset
                                dst-designation std-designation)))))
      (else (vector (vector-ref ranges i))))))

;;; ;;;;;;;;;
;;; Init.
;;; ;;;;;;;;;
(reload-timezone-data!)