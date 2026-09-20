;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-record-type <leap-seconds-info>
  (leap-seconds-info expiry last-updated occurences)
  leap-seconds-info?
  (expiry leap-seconds-info-expiry)
  (last-updated leap-seconds-info-last-updated)
  (occurences leap-seconds-info-occurences))

(define-record-type <leap-second-occurence>
  (leap-second-occurence time delta-tai)
  leap-second-occurence?
  (time leap-second-occurence-time)
  (delta-tai leap-second-occurence-delta-tai))

(define (leap-seconds-info->datum info)
  (list 'leap-seconds
        'expiry (leap-seconds-info-expiry info)
        'last-updated (leap-seconds-info-last-updated info)
        'transitions
        (vector-map leap-second-occurence->datum
                    (leap-seconds-info-occurences info))))

(define (leap-second-occurence->datum transition)
  (list 'transition 'ntp
        (leap-second-occurence-time transition)
        'delta-TAI
        (leap-second-occurence-delta-tai transition)))

(define (delta-tai-for-timestamp ntp occurences)
  (let search ((l 0) (r (- (vector-length occurences) 1)))
    (if (> l r)
        (error "internal error" ntp occurences)
        (let* ((i (+ l (floor-quotient (- r l) 2)))
               (here (vector-ref occurences i)))
          (cond
            ((and (zero? i) (< ntp (leap-second-occurence-time here)))
             10)
            ((and (= i (- (vector-length occurences) 1))
                  (<= (leap-second-occurence-time here) ntp))
             (leap-second-occurence-delta-tai here))
            ((< ntp (leap-second-occurence-time here))
             (search l (- i 1)))
            ((and (<= (leap-second-occurence-time here) ntp)
                  (< ntp (leap-second-occurence-time
                          (vector-ref occurences (+ i 1)))))
             (leap-second-occurence-delta-tai here))
            (else (search (+ i 1) r)))))))

;;;;;;;;;;;
;;; Parsing
;;;;;;;;;;;

(define (read-leap-seconds filename/port)
  (let* ((alist (read-leap-seconds* filename/port))
         (expiry (cond
                   ((assq 'expiry alist) => cdr)
                   (else #f)))
         (last-updated (cond
                         ((assq 'last-updated alist) => cdr)
                         (else #f))))
    (leap-seconds-info expiry last-updated
                       (collect-transitions alist))))

(define (collect-transitions alist)
  (define number-of-transitions
    (let loop ((alist alist) (i 0))
      (cond
        ((null? alist) i)
        ((number? (caar alist)) (loop (cdr alist) (+ i 1)))
        (else (loop (cdr alist) i)))))
  (define v (make-vector number-of-transitions))
  (let loop ((alist alist)
             (i (- (vector-length v) 1)))
    (cond
      ((negative? i) v)
      ((not (number? (caar alist)))
       (loop (cdr alist) i))
      (else
       (vector-set! v i
                    (leap-second-occurence (caar alist)
                                           (cdar alist)))
       (loop (cdr alist) (- i 1))))))

(define (read-leap-seconds* filename/port)
  (cond
    ((input-port? filename/port)
     (read-leap-second-line filename/port '()))
    ((string? filename/port)
     (call-with-input-file filename/port
       (lambda (port) (read-leap-second-line port '()))))
    (else (error "not a filename or an input port" filename/port))))

(define (skip-whitespace! port)
  (let ((ch (peek-char port)))
    (when (and (char? ch) (char-whitespace? ch))
      (read-char port)
      (skip-whitespace! port))))

(define (skip-to-newline! port)
  (let ((ch (read-char port)))
    (unless (or (eof-object? ch) (char=? ch #\newline))
      (skip-to-newline! port))))

(define (check-comment port acc)
  (let ((ch (peek-char port)))
    (cond
      ((eqv? ch #\$)
       (read-char port)
       (read-single 'last-updated port acc))
      ((eqv? ch #\@)
       (read-char port)
       (read-single 'expiry port acc))
      (else (skip-to-newline! port)
            (read-leap-second-line port acc)))))

(define (read-single name port acc)
  (skip-whitespace! port)
  (let ((number (read-number port)))
    (skip-to-newline! port)
    (read-leap-second-line port
                           (cons (cons name number)
                                 acc))))

(define (read-number port)
  (let loop ((n 0))
    (let ((ch (peek-char port)))
      (cond
        ((eof-object? ch) n)
        ((char<=? #\0 ch #\9)
         (read-char port)
         (loop (+ (* n 10) (digit-value ch))))
        (else n)))))

(define (read-leap-second-line port acc)
  (skip-whitespace! port)
  (let ((ch (peek-char port)))
    (cond
     ((eof-object? ch) acc)
     ((char-whitespace? ch)
      (read-char port)
      (read-leap-second-line port acc))
     ((char=? ch #\#)
      (read-char port)
      (check-comment port acc))
     (else
      (let ((ntp-timestamp (read-number port)))
        (skip-whitespace! port)
        (let ((dtai (read-number port)))
          (skip-to-newline! port)
          (read-leap-second-line port
                                 (cons (cons ntp-timestamp dtai)
                                       acc))))))))



