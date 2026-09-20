;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define (remove-leap-seconds transitions leap-seconds)
  ;; Given a vector of transitions from a TZif file, and the
  ;; leap second records from the same file, remove the leap seconds
  ;; from the transitions, turning them into POSIX timestamps
  ;; (UTC seconds from the epoch).
  (define new-transitions (vector-copy transitions))
  (define (find-next-adjustment time leap-i)
    ;; Find the next leap second adjustment.
    (let loop ((leap-i leap-i))
      (cond
        ((= leap-i (vector-length leap-seconds))
         (- leap-i 1))
        ((<= time
             (tzif-leap-second-occurence
              (vector-ref leap-seconds leap-i)))
         (- leap-i 1))
        (else (loop (+ leap-i 1))))))
  (cond
    ((zero? (vector-length leap-seconds))
     new-transitions)
    ;; Skip to the first transition that is greater than the first
    ;; leap second occurence.
    (else
     (let* ((first (tzif-leap-second-occurence
                    (vector-ref leap-seconds 0)))
            (first-to-adjust
             (do ((i 0 (+ i 1)))
                 ((or (= i (vector-length new-transitions))
                      (<= first
                          (tzif-transition-time
                           (vector-ref transitions i))))
                  i))))
       ;; Loop through the remaining transitions, removing leap seconds
       ;; from them, and adjusting the offset when a new leap second
       ;; is hit.
       (let loop ((leap-i 0)
                  (tr-i first-to-adjust))
         (cond
          ((= tr-i (vector-length transitions))
           new-transitions)
          (else
           (let* ((transition (vector-ref transitions tr-i))
                  (time (tzif-transition-time transition))
                  (leap-i (find-next-adjustment time leap-i))
                  (corr (tzif-leap-second-correction
                         (vector-ref leap-seconds leap-i))))
             (vector-set!
              new-transitions
              tr-i
              (tzif-transition (- time corr)
                               (tzif-transition-offset transition)
                               (tzif-transition-dst? transition)
                               (tzif-transition-designation transition)
                               (tzif-transition-standard? transition)
                               (tzif-transition-universal-time? transition)))
             (loop leap-i (+ tr-i 1))))))))))

(define (add-leap-seconds tzif-transitions target-leap-seconds)
  ;; Given a set of transitions in POSIX time (no leap seconds),
  ;; returns a vector with the leap seconds in POSIX TAI time
  ;; (TAI seconds since the UNIX epoch).
  (define new-transitions (vector-copy tzif-transitions))
  (define (POSIX-occurence i)
    (ntp-timestamp->unix-timestamp
     (leap-second-occurence-time
      (vector-ref target-leap-seconds i))))
  (define (POSIX-adjustment i)
    (leap-second-occurence-delta-tai
     (vector-ref target-leap-seconds i)))
  (define (find-index time leap-i)
    (do ((leap-i leap-i (+ leap-i 1)))
        ((or (= leap-i (vector-length target-leap-seconds))
             (<= time (POSIX-occurence leap-i)))
         (max 0 (- leap-i 1)))))
  (let loop ((tr-i 0)
             (leap-i 0))
    (if (= (vector-length tzif-transitions) tr-i)
        new-transitions
        (let* ((transition (vector-ref tzif-transitions tr-i))
               (time (tzif-transition-time transition))
               (leap-i (find-index time leap-i)))
          (vector-set!
           new-transitions
           tr-i
           (tzif-transition (+ time (POSIX-adjustment leap-i))
                            (tzif-transition-offset transition)
                            (tzif-transition-dst? transition)
                            (tzif-transition-designation transition)
                            (tzif-transition-standard? transition)
                            (tzif-transition-universal-time? transition)))
          (loop (+ tr-i 1) leap-i)))))


