;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define (ASCII-alphabetic? b8)
  (or (<= (char->integer #\A) b8 (char->integer #\Z))
      (<= (char->integer #\a) b8 (char->integer #\z))))
(define (ASCII-digit? b8)
  (or (<= (char->integer #\0) b8 (char->integer #\9))))
(define (ASCII-digit-value b8)
  (- b8 (char->integer #\0)))

(define (parse-timezone-name port)
  (let ((first (peek-u8 port)))
    (cond
      ((eof-object? first) #f)
      ((= first (char->integer #\<))
       (read-u8 port)
       (parse-quoted-timezone-name port))
      ((ASCII-alphabetic? first)
       (parse-unquoted-timezone-name port))
      (else #f))))

(define (parse-quoted-timezone-name port)
  (call-with-port (open-output-bytevector)
    (lambda (buffer)
      (let loop ()
        (let ((b8 (read-u8 port)))
          (cond
            ((eof-object? b8)
             (error "EOF reading quoted timezone name" port))
            ((= b8 (char->integer #\>))
             (utf8->string (get-output-bytevector buffer)))
            (else
             (write-u8 b8 buffer)
             (loop))))))))

(define (parse-unquoted-timezone-name port)
  (call-with-port (open-output-bytevector)
    (lambda (buffer)
      (let loop ()
        (let ((b8 (peek-u8 port)))
          (cond
           ((and (integer? b8) (ASCII-alphabetic? b8))
            (read-u8 port)
            (write-u8 b8 buffer)
            (loop))
           (else
            (utf8->string (get-output-bytevector buffer)))))))))

(define-syntax %prog
  (syntax-rules (define*)
    ((_ return (define* a b) rest ...)
     (let ((a b))
       (%prog return rest ...)))
    ((_ return e1 e2 e3 ...)
     (begin e1 (%prog return e2 e3 ...)))
    ((_ return e) e)))

(define-syntax prog
  (syntax-rules ()
    ((_ return commands ...)
     (call/cc (lambda (return) (%prog return commands ...))))))

(define (parse-number port)
  (let loop ((n 0))
    (let ((b8 (peek-u8 port)))
      (if (and (integer? b8)
               (ASCII-digit? b8))
          (begin
            (read-u8 port)
            (loop (+ (* n 10) (ASCII-digit-value b8))))
          n))))

(define (parse-number* emsg port)
  (let ((b8 (peek-u8 port)))
    (unless (and (integer? b8) (ASCII-digit? b8))
      (error emsg port))
    (parse-number port)))

(define (combine-digits dig1 dig2)
  (if dig2
      (+ (* dig1 10) dig2)
      dig1))

(define (offset-finished? port)
  (let ((sep (peek-u8 port)))
    (or (eof-object? sep)
        (not (= sep (char->integer #\:))))))

(define (offset-starts-here? port)
  (let ((b8 (peek-u8 port)))
    (and (integer? b8)
         (or (= b8 (char->integer #\+))
             (= b8 (char->integer #\-))
             (ASCII-digit? b8)))))

(define-record-type <tz-offset>
  (tz-offset sign? hour minute second)
  tz-offset?
  (sign? tz-offset-sign-negative?)
  (hour tz-offset-hour)
  (minute tz-offset-minute)
  (second tz-offset-second))

(define (parse-offset port)
  ;;; FIXME: This is a pretty disgusting parser.
  ;;; The TZ format was clearly designed to be parsed by a sequential
  ;;; program like this one.
  (prog return
    (define* hour-sign (peek-u8 port))
    (cond
      ((eof-object? hour-sign)
       (error "EOF reading hour sign" port))
      ((= hour-sign (char->integer #\-))
       (read-u8 port)
       (set! hour-sign #t))
      ((= hour-sign (char->integer #\+))
       (read-u8 port)
       (set! hour-sign #f))
      (else
       (set! hour-sign #f)))

    (define* hour (parse-number* "expected hour in offset" port))
    (when (offset-finished? port)
      (return (tz-offset hour-sign hour 0 0)))
    (read-u8 port)   ; ":"

    (define* minute (parse-number port))
    (when (offset-finished? port)
      (return (tz-offset hour-sign hour minute 0)))
    (read-u8 port) ; ":"

    (define* second (parse-number port))
    (tz-offset hour-sign hour minute second)))

(define (assert-separator! port char)
  (unless (eqv? (peek-u8 port) (char->integer char))
    (error "expected separator" char port))
  (read-u8 port))

(define-record-type <tz-month-week-day>
  (tz-month-week-day month week day)
  tz-month-week-day?
  (month tz-month-week-day:month)
  (week tz-month-week-day:week)
  (day tz-month-week-day:day))

(define (parse-month-week-day port)
  (let* ((month (parse-number port))
         (ignored (assert-separator! port #\.))
         (week (parse-number port))
         (ignored (assert-separator! port #\.))
         (day (parse-number port)))
    (tz-month-week-day month week day)))

(define-record-type <tz-julian-without-leap-day>
  (tz-julian-without-leap-day date)
  tz-julian-without-leap-day?
  (date tz-julian-without-leap-day:date))

(define-record-type <tz-julian-with-leap-day>
  (tz-julian-with-leap-day date)
  tz-julian-with-leap-day?
  (date tz-julian-with-leap-day:date))

(define (parse-date port)
  (let ((b8 (peek-u8 port)))
    (cond
      ((eof-object? b8)
       (error "EOF where date was expected" b8))
      ((= b8 (char->integer #\J))
       (read-u8 port)
       (tz-julian-without-leap-day (parse-number port)))
      ((= b8 (char->integer #\M))
       (read-u8 port)
       (parse-month-week-day port))
      (else (tz-julian-with-leap-day (parse-number port))))))

(define (parse-offset-after-date port)
  (let ((b8 (peek-u8 port)))
    (if (eqv? b8 (char->integer #\/))
        (begin
          (read-u8 port)
          (parse-offset port))
        (tz-offset #f 2 0 0))))

(define-record-type <tz-transition>
  (tz-transition date time)
  tz-transition?
  (date tz-transition-date)
  (time tz-transition-time))

(define (parse-rules port)
  (let* ((date1 (parse-date port))
         (time1 (parse-offset-after-date port))
         (unused (assert-separator! port #\,))
         (date2 (parse-date port))
         (time2 (parse-offset-after-date port)))
    (values (tz-transition date1 time1)
            (tz-transition date2 time2))))

(define-record-type <tz-string>
  (tz-string stdname stdoffset
             dstname dstoffset
             change-to-dst
             change-to-std)
  tz-string?
  (stdname tz-string-stdname)
  (stdoffset tz-string-stdoffset)
  (dstname tz-string-dstname)
  (dstoffset tz-string-dstoffset)
  (change-to-dst tz-string-change-to-dst)
  (change-to-std tz-string-change-to-std))

(define (parse-tz input)
  (cond
    ((input-port? input)
     (parse-TZ-bytes input))
    ((bytevector? input)
     (call-with-port (open-input-bytevector input)
       parse-TZ-bytes))
    ((string? input)
     (call-with-port (open-input-bytevector
                      (string->utf8 input))
       parse-TZ-bytes))
    (else (error "invalid input" input))))

(define (parse-TZ-bytes port)
  (prog return
    (define* stdname (parse-timezone-name port))
    (define* offset (parse-offset port))
    (define* dstname (parse-timezone-name port))
    (when (not dstname)
      (return (tz-string stdname offset #f #f #f #f)))

    (define* dstoffset
      (if (offset-starts-here? port)
          (parse-offset port)
          (let ((offset-sign-negative? (tz-offset-sign-negative? offset))
                (hour (tz-offset-hour offset)))
            (cond
              (offset-sign-negative?
               (tz-offset #t (+ hour 1)
                          (tz-offset-minute offset)
                          (tz-offset-second offset)))
              ((zero? hour)
               (tz-offset #t 1
                          (tz-offset-minute offset)
                          (tz-offset-second offset)))
              (else
               (tz-offset #f (- hour 1)
                          (tz-offset-minute offset)
                          (tz-offset-second offset)))))))

    (unless (eqv? (peek-u8 port) (char->integer #\,))
      (return (tz-string stdname offset dstname dstoffset #f #f)))
    (read-u8 port)

    (let-values (((change-to-dst change-to-std) (parse-rules port)))
      (tz-string stdname offset dstname dstoffset
                 change-to-dst change-to-std))))

(define (tz-offset->datum offset)
  (if (not offset)
      #f
      (list (tz-offset-sign-negative? offset)
            (tz-offset-hour offset)
            (tz-offset-minute offset)
            (tz-offset-second offset))))

(define (tz-transition->datum transition)
  (if (not transition)
      #f
      (list (tz-date->datum (tz-transition-date transition))
            (tz-offset->datum (tz-transition-time transition)))))

(define (tz-date->datum date)
  (cond
    ((not date) #f)
    ((tz-month-week-day? date)
     (list 'M
           (tz-month-week-day:month date)
           (tz-month-week-day:week date)
           (tz-month-week-day:day date)))
    ((tz-julian-without-leap-day? date)
     (list 'J (tz-julian-without-leap-day:date date)))
    ((tz-julian-with-leap-day? date)
     (list 'J/L (tz-julian-with-leap-day:date date)))))

(define (tz-string->datum tz-string)
  (list (tz-string-stdname tz-string)
        (tz-offset->datum (tz-string-stdoffset tz-string))
        (tz-string-dstname tz-string)
        (tz-offset->datum (tz-string-dstoffset tz-string))
        (tz-transition->datum (tz-string-change-to-dst tz-string))
        (tz-transition->datum (tz-string-change-to-std tz-string))))


