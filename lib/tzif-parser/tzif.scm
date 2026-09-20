;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(cond-expand
  ((not (library (srfi 281)))
   (define (bytevector-u32-ref bv k endianness)
     ;; XXX: Endianness is always 'big
     (do ((i k (+ i 1))
          (acc 0 (+ (* acc 256)
                    (bytevector-u8-ref bv i))))
         ((= i (+ k 4)) acc)))
   (define (bytevector-s32-ref bv k endianness)
     (let ((value (bytevector-u32-ref bv k endianness)))
       (if (>= value #x80000000)
           (- value #x100000000)
           value)))
   (define (bytevector-u64-ref bv k endianness)
     (do ((i k (+ i 1))
          (acc 0 (+ (* acc 256)
                    (bytevector-u8-ref bv i))))
         ((= i (+ k 8)) acc)))
   (define (bytevector-s64-ref bv k endianness)
     (let ((value (bytevector-u64-ref bv k endianness)))
       (if (>= value #x8000000000000000)
           (- value #x10000000000000000)
           value))))
  (else))

(define (tzif-file? filename/port)
  (define (check port)
    (equal? (read-bytevector 4 port)
            #u8(#x54 #x5A #x69 #x66)))
  (cond
    ((input-port? filename/port)
     (check filename/port))
    ((string? filename/port)
     (call-with-port (open-binary-input-file filename/port)
       check))
    (else (error "not a filename or a port"
                 filename/port))))

(define (read-tzif-simple filename/port)
  (cond
    ((and (binary-port? filename/port)
          (input-port? filename/port))
     (read-tzif-simple* #f filename/port))
    (else
     (call-with-port (open-binary-input-file filename/port)
       (lambda (port)
         (read-tzif-simple* filename/port port))))))

;;;;;;;;;
;;; Utils
;;;;;;;;;

(define (read-bytevector* filename blame number port)
  (let ((bv (read-bytevector number port)))
    (unless (= (bytevector-length bv) number)
      (error "truncated trying to read" filename blame number port))
    bv))

(define (read-u32 filename blame port)
  (let ((bv (read-bytevector* filename blame 4 port)))
    (bytevector-u32-ref bv 0 'big)))

(define (read-s32 filename blame port)
  (let ((bv (read-bytevector* filename blame 4 port)))
    (bytevector-s32-ref bv 0 'big)))

(define (read-s64 filename blame port)
  (let ((bv (read-bytevector* filename blame 8 port)))
    (bytevector-s64-ref bv 0 'big)))

(define (read-u8* filename blame port)
  (let ((u8 (read-u8 port)))
    (when (eof-object? u8)
      (error "truncated trying to read" blame port))
    u8))

;;;;;;;;;
;;; Main parsers
;;;;;;;;;

(define (assert-magic-string! filename port)
  (unless (equal? (read-bytevector* filename "magic string" 4 port)
                  (string->utf8 "TZif"))
    (error "file does not start with magic string" filename port)))

(define (read-version filename port)
  (let ((version (read-u8* filename "version" port)))
    (case version
      ((0) 1)
      ((#x32) 2)
      ((#x33) 3)
      ((#x34) 4)
      (else (display "WARNING: the file '" (current-error-port))
            (display filename (current-error-port))
            (display "' has unknown version " (current-error-port))
            (display version (current-error-port))
            (display ": treating it as version 4\n")
            4))))

(define-record-type <tzif-header>
  (tzif-header version isutcnt isstdcnt leapcnt timecnt typecnt charcnt)
  tzif-header?
  (version tzif-header-version)
  (isutcnt tzif-header-isutcnt)
  (isstdcnt tzif-header-isstdcnt)
  (leapcnt tzif-header-leapcnt)
  (timecnt tzif-header-timecnt)
  (typecnt tzif-header-typecnt)
  (charcnt tzif-header-charcnt))

(define (tzif-header->datum header)
  (list 'TZif
        'version (tzif-header-version header)
        'isutcnt (tzif-header-isutcnt header)
        'istdcnt (tzif-header-isstdcnt header)
        'leapcnt (tzif-header-leapcnt header)
        'timecnt (tzif-header-timecnt header)
        'typecnt (tzif-header-typecnt header)
        'charcnt (tzif-header-charcnt header)))

(define (read-header filename port)
  (assert-magic-string! filename port)
  (let ((version (read-version filename port)))
    (read-bytevector* filename "reserved bytes" 15 port)
    (let* ((isutcnt (read-u32 filename "isutcnt" port))
           (isstdcnt (read-u32 filename "isstdcnt" port))
           (leapcnt (read-u32 filename "leapcnt" port))
           (timecnt (read-u32 filename "timecnt" port))
           (typecnt (read-u32 filename "typecnt" port))
           (charcnt (read-u32 filename "charcnt" port)))
      (tzif-header version isutcnt isstdcnt leapcnt timecnt typecnt charcnt))))

(define (read-transition-times reader timecnt filename port)
  (do ((v (make-vector timecnt))
       (i 0 (+ i 1)))
      ((= i timecnt) v)
    (vector-set! v i (reader filename "transition time" port))))

(define-record-type <local-time-type>
  (local-time-type offset dst? desigidx)
  local-time-type?
  (offset local-time-type-offset)
  (dst? local-time-type-dst?)
  (desigidx local-time-type-desigidx))

(define (read-local-time-type-records typecnt filename port)
  (do ((v (make-vector typecnt))
       (i 0 (+ i 1)))
      ((= i typecnt) v)
    (let* ((offset (read-s32 filename "local time type offset" port))
           (dst (read-u8* filename "local time type dst flag" port))
           (desigidx (read-u8* filename "designation offset" port)))
    (vector-set! v i (local-time-type offset
                                      (= dst 1) ; 0 = #f, 1 = #t
                                      desigidx)))))

(define-record-type <tzif-leap-second>
  (tzif-leap-second occurence correction)
  tzif-leap-second?
  (occurence tzif-leap-second-occurence)
  (correction tzif-leap-second-correction))

(define (tzif-leap-second->datum leap-second)
  (list 'leap-second-occurs-at
        (tzif-leap-second-occurence leap-second)
        (tzif-leap-second-correction leap-second)))

(define-record-type <tzif-leap-seconds-record>
  (tzif-leap-seconds-record vector expiry)
  tzif-leap-seconds-record?
  (vector tzif-leap-seconds-record-vector)
  (expiry tzif-leap-seconds-record-expiry))

(define (vector->tzif-leap-seconds-record leap-seconds)
  (define len (vector-length leap-seconds))
  (define expiry
    ;; If the last two leap-second entries have the same correction,
    ;; then the leap-second table expires at the time of the
    ;; last occurence.
    (if (< len 2)
        #f
        (let ((last (vector-ref leap-seconds (- len 1)))
              (next-to-last (vector-ref leap-seconds (- len 2))))
          (if (= (tzif-leap-second-correction last)
                 (tzif-leap-second-correction next-to-last))
              (tzif-leap-second-occurence last)
              #f))))
  (if expiry
      (tzif-leap-seconds-record (vector-copy leap-seconds
                                             0
                                             (- len 1))
                                expiry))
      (tzif-leap-seconds-record leap-seconds #f))

(define (tzif-leap-seconds-record->datum record)
  `(leap-seconds ,(vector-map tzif-leap-second->datum
                              (tzif-leap-seconds-record-vector record))
                 expiry ,(tzif-leap-seconds-record-expiry record)))

(define (read-leap-second-records reader leapcnt filename port)
  (do ((v (make-vector leapcnt))
       (i 0 (+ i 1)))
      ((= i leapcnt) v)
    (let* ((occurence (reader filename "leap second occurence" port))
           (correction (read-s32 filename "leap second correction" port)))
      (vector-set! v i (tzif-leap-second occurence correction)))))

(define (allowed-in-designation? b8)
  (or (<= #x30 b8 #x39) ; ASCII numeral
      (<= #x41 b8 #x5A) ; ASCII capital letter
      (<= #x61 b8 #x7A) ; ASCII lowercase letter
      (= b8 #x2B)       ; ASCII +
      (= b8 #x2D)       ; ASCII -
))

(define (read-designation filename port designations start-idx)
  (call-with-port (open-output-string)
    (lambda (buffer)
      (let loop ((i start-idx))
        (if (= i (bytevector-length designations))
            (error "designation does not end in a NUL terminator"
                   filename
                   port
                   designations
                   start-idx)
            (let ((u8 (bytevector-u8-ref designations i)))
              (cond
                ((zero? u8) (get-output-string buffer))
                ((allowed-in-designation? 88)
                 (write-char (integer->char u8) buffer)
                 (loop (+ i 1)))
                (else
                 (display "WARNING: byte " (current-error-port))
                 (write u8 (current-error-port))
                 (display " should not be in a designation. \
Use a systematic name instead." (current-error-port))
                 #f))))))))

(define-record-type <tzif-transition>
  (tzif-transition time
                   offset
                   dst?
                   designation
                   standard?
                   universal-time?)
  tzif-transition?
  (time tzif-transition-time)
  (offset tzif-transition-offset)
  (dst? tzif-transition-dst?)
  (designation tzif-transition-designation)
  (standard? tzif-transition-standard?)
  (universal-time? tzif-transition-universal-time?))

(define (tzif-transition->datum transition)
  (list 'transition-occurs-at
        (tzif-transition-time transition)
        'offset
        (tzif-transition-offset transition)
        'dst?
        (tzif-transition-dst? transition)
        'designation
        (tzif-transition-designation transition)
        'standard?
        (tzif-transition-standard? transition)
        'universal-time?
        (tzif-transition-universal-time? transition)))

(define (read-block block-version-number header filename port)
  (let* ((reader (if (= block-version-number 1)
                     read-s32
                     read-s64))
         (transition-times
          (read-transition-times reader
                                 (tzif-header-timecnt header)
                                 filename
                                 port))
         (transition-types
          (read-bytevector* filename
                            "transition types"
                            (tzif-header-timecnt header)
                            port))
         (local-time-type-records
          (read-local-time-type-records (tzif-header-typecnt header)
                                        filename
                                        port))
         (designations (read-bytevector* filename
                                         "designations"
                                         (tzif-header-charcnt header)
                                         port))
         (leap-second-records
          (read-leap-second-records reader
                                    (tzif-header-leapcnt header)
                                    filename
                                    port))
         (standard/wall
          (read-bytevector* filename
                            "standard/wall indicators"
                            (tzif-header-isstdcnt header)
                            port))
         (ut/local
          (read-bytevector* filename
                            "ut/local indicators"
                            (tzif-header-isutcnt header)
                            port)))
    (define (derive-transition time typeidx)
      (let* ((type (vector-ref local-time-type-records typeidx))
             (designation (read-designation filename
                                            port
                                            designations
                                            (local-time-type-desigidx
                                             type)))
             (standard?
              (if (zero? (bytevector-length standard/wall))
                  #f
                  (= (bytevector-u8-ref standard/wall typeidx) 1)))
             (universal-time?
              (if (zero? (bytevector-length ut/local))
                  #f
                  (= (bytevector-u8-ref ut/local typeidx) 1))))
        (tzif-transition time
                         (local-time-type-offset type)
                         (local-time-type-dst? type)
                         designation
                         standard?
                         universal-time?)))
    (do ((transitions (make-vector (vector-length transition-times)))
         (first (derive-transition #f 0))
         (leap-seconds-record (vector->tzif-leap-seconds-record
                               leap-second-records))
         (i 0 (+ i 1)))
        ((= i (vector-length transitions))
         (values first transitions leap-seconds-record))
      (vector-set! transitions i
                   (derive-transition (vector-ref transition-times i)
                                      (bytevector-u8-ref transition-types
                                                         i))))))

(define (version-1-block-length header)
  (+ (* 4 (tzif-header-timecnt header))
     (tzif-header-timecnt header)
     (* 6 (tzif-header-typecnt header))
     (tzif-header-charcnt header)
     (* (tzif-header-leapcnt header) 8)
     (tzif-header-isstdcnt header)
     (tzif-header-isutcnt header)))

(define (read-footer filename port)
  (unless (eqv? (read-u8* filename "footer start" port) #x0A)
    (error "footer not found" filename port))
  (call-with-port (open-output-bytevector)
    (lambda (buffer)
      (let loop ()
        (let ((b8 (read-u8* filename "tzfile footer" port)))
          (cond
            ((= b8 #x0A)
             (let ((buffer (get-output-bytevector buffer)))
               (if (zero? (bytevector-length buffer))
                   #f
                   (call-with-port (open-input-bytevector buffer)
                     parse-tz))))
            (else
             (write-u8 b8 buffer)
             (loop))))))))

(define (read-tzif-simple* filename port)
  (let ((header (read-header filename port)))
    (if (= (tzif-header-version header) 1)
        (let-values (((first transitions leap-seconds)
                      (read-block 1 header filename port)))
          (values header first transitions leap-seconds #f))
        (begin
          (read-bytevector* filename
                            "skipped version 1 block"
                            (version-1-block-length header)
                            port)
          (let*-values (((header) (read-header filename port))
                        ((first transitions leap-seconds)
                         (read-block (tzif-header-version header)
                                     header
                                     filename
                                     port))
                        ((footer) (read-footer filename port)))
            (values header first transitions leap-seconds footer))))))

(define (tzif-transition-local-time-unspecified? transition)
  (string=? (tzif-transition-designation transition)
            "-00"))

