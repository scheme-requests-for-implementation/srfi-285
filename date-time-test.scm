;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-FileCopyrightText: 2024-2026 Arvydas Silanskas
;;; SPDX-License-Identifier: MIT

(import (scheme base)
        (scheme file)
        (srfi 285 clock-time)
        (srfi 285 current-time)
        (srfi 285 date)
        (srfi 285 gregorian)
        (srfi 285 moment)
        (srfi 285 time-delta)
        (srfi 285 timezone)
        (srfi 285 timestamp)
        (srfi 285 tzdb)
        (srfi 64))

;; test helpers
(define (timestamp-equal? t1 t2)
    (and (date=? (timestamp-date t1) (timestamp-date t2))
         (= (timestamp-hour t1) (timestamp-hour t2))
         (= (timestamp-minute t1) (timestamp-minute t2))
         (= (timestamp-second t1) (timestamp-second t2))
         #;(eq? (timestamp-timezone t1) (timestamp-timezone t2))))

(define (dt-equal? dt1 dt2)
    (and (= (time-delta-years dt1) (time-delta-years dt2))
         (= (time-delta-months dt1) (time-delta-months dt2))
         (= (time-delta-weeks dt1) (time-delta-weeks dt2))
         (= (time-delta-days dt1) (time-delta-days dt2))
         (= (time-delta-hours dt1) (time-delta-hours dt2))
         (= (time-delta-minutes dt1) (time-delta-minutes dt2))
         (= (time-delta-seconds dt1) (time-delta-seconds dt2))))

(test-begin "Date-Time")

;;; dates

(test-group "Date constructor, getters"
    (test-assert (date? (make-date 2020 1 1)))
    (test-assert (date? (make-date 2020 2 29)))
    (test-assert (date? (make-date 2000 2 29)))
    (test-error (make-date 2020 1 32))
    (test-error (make-date 2020 1 -1))
    (test-error (make-date 2020 0 1))
    (test-error (make-date 2020 13 1))
    (test-error (make-date 2020.1 1 1))
    (test-error (make-date 2020 1.1 1))
    (test-error (make-date 2020 1 1.1))
    (test-error (make-date 1900 2 29))
    (test-equal 2000 (date-year (make-date 2000 1 2)))
    (test-equal 1 (date-month (make-date 2000 1 2)))
    (test-equal 2 (date-day (make-date 2000 1 2)))
    (let-values (((y m d) (date-ymd (make-date 2000 1 2))))
      (test-equal 2000 y)
      (test-equal 1 m)
      (test-equal 2 d)))

(test-group "Date->ISO-8601"
    (test-equal "2020-01-01" (date->iso-8601 (make-date 2020 1 1)))
    (test-equal "2020-11-12" (date->iso-8601 (make-date 2020 11 12)))
    (test-equal "0000-01-01" (date->iso-8601 (make-date 0 1 1)))
    (test-equal "-0001-01-01" (date->iso-8601 (make-date -1 1 1)))
    (test-equal "99999-01-01" (date->iso-8601 (make-date 99999 1 1))))

(test-group "Week & weekday calculation"
  (test-equal 7 (date-iso-weekday (make-date 2025 10 12)))
  (test-equal 1 (date-iso-weekday (make-date 2025 10 13)))
  (test-equal 42 (date-iso-week (make-date 2025 10 14)))
  (test-equal 2025 (date-iso-week-year (make-date 2025 10 14)))
  (test-equal 1 (date-iso-week (make-date 2008 12 29)))
  (test-equal 2009 (date-iso-week-year (make-date 2008 12 29)))
  (test-equal 53 (date-iso-week (make-date 2010 1 2)))
  (test-equal 2009 (date-iso-week-year (make-date 2010 1 2))))

(test-group "Rata Die"
    (test-equal 1 (date->rata-die (make-date 1 1 1)))
    (test-equal 739539 (date->rata-die (make-date 2025 10 15)))
    (test-equal -1 (date->rata-die (make-date 0 12 30)))
    (test-equal -365 (date->rata-die (make-date 0 1 1)))
    (test-equal -366 (date->rata-die (make-date -1 12 31)))

    (test-assert (date=? (make-date 1 1 1) (rata-die->date 1)))
    (test-assert (date=? (make-date 2025 10 15) (rata-die->date 739539)))
    (test-assert (date=? (make-date 0 12 30) (rata-die->date -1)))
    (test-assert (date=? (make-date 0 1 1) (rata-die->date -365)))
    (test-assert (date=? (make-date -1 12 31) (rata-die->date -366))))

(test-group "MJD"
    (test-equal 0 (date->mjd (make-date 1858 11 17)))
    (test-equal 60965 (date->mjd (make-date 2025 10 17)))

    (test-assert (date=? (make-date 1858 11 17) (mjd->date 0)))
    (test-assert (date=? (make-date 2025 10 17) (mjd->date 60965))))

(test-group "Date comparators"
    (test-assert (not (date=? (make-date 2021 1 1) (make-date 2020 1 1))))
    (test-assert (not (date=? (make-date 2020 2 1) (make-date 2020 1 1))))
    (test-assert (not (date=? (make-date 2020 1 2) (make-date 2020 1 1))))
    (test-assert (date=? (make-date 2020 1 1) (make-date 2020 1 1)))

    (test-assert (date<? (make-date 2020 2 2) (make-date 2021 1 1)))
    (test-assert (date<? (make-date 2020 2 2) (make-date 2020 3 1)))
    (test-assert (date<? (make-date 2020 2 2) (make-date 2020 2 3)))
    (test-assert (not (date<? (make-date 2020 2 2) (make-date 2020 2 2))))
    (test-assert (not (date<? (make-date 2021 1 1) (make-date 2020 2 2))))
    (test-assert (not (date<? (make-date 2020 3 1) (make-date 2020 2 2))))
    (test-assert (not (date<? (make-date 2020 2 3) (make-date 2020 2 2))))

    (test-assert (date<=? (make-date 2020 2 2) (make-date 2021 1 1)))
    (test-assert (date<=? (make-date 2020 2 2) (make-date 2020 3 1)))
    (test-assert (date<=? (make-date 2020 2 2) (make-date 2020 2 3)))
    (test-assert (date<=? (make-date 2020 2 2) (make-date 2020 2 2)))
    (test-assert (not (date<=? (make-date 2021 1 1) (make-date 2020 2 2))))
    (test-assert (not (date<=? (make-date 2020 3 1) (make-date 2020 2 2))))
    (test-assert (not (date<=? (make-date 2020 2 3) (make-date 2020 2 2))))

    (test-assert (date>? (make-date 2021 1 1) (make-date 2020 2 2)))
    (test-assert (date>? (make-date 2020 3 1) (make-date 2020 2 2)))
    (test-assert (date>? (make-date 2020 2 3) (make-date 2020 2 2)))
    (test-assert (not (date>? (make-date 2020 2 2) (make-date 2020 2 2))))
    (test-assert (not (date>? (make-date 2020 2 2) (make-date 2021 1 1))))
    (test-assert (not (date>? (make-date 2020 2 2) (make-date 2020 3 1))))
    (test-assert (not (date>? (make-date 2020 2 2) (make-date 2020 2 3))))

    (test-assert (date>=? (make-date 2021 1 1) (make-date 2020 2 2)))
    (test-assert (date>=? (make-date 2020 3 1) (make-date 2020 2 2)))
    (test-assert (date>=? (make-date 2020 2 3) (make-date 2020 2 2)))
    (test-assert (date>=? (make-date 2020 2 2) (make-date 2020 2 2)))
    (test-assert (not (date>=? (make-date 2020 2 2) (make-date 2021 1 1))))
    (test-assert (not (date>=? (make-date 2020 2 2) (make-date 2020 3 1))))
    (test-assert (not (date>=? (make-date 2020 2 2) (make-date 2020 2 3)))))

;; clock-time

(test-group "Clock time"
    (test-assert (clock-time? (make-clock-time 23 0 11/10)))
    (test-error (make-clock-time 24 1 1))
    (test-error (make-clock-time -1 1 1))
    (test-error (make-clock-time 23 100 1))
    (test-error (make-clock-time 23 -1 1))
    (test-error (make-clock-time 23 1 -1))
    (test-error (make-clock-time 23 1 1+1i))

    (test-equal 23 (clock-time-hour (make-clock-time 23 1 2)))
    (test-equal 1 (clock-time-minute (make-clock-time 23 1 2)))
    (test-equal 2 (clock-time-second (make-clock-time 23 1 2)))

    (let-values (((h m s) (clock-time-hms (make-clock-time 23 1 2))))
      (test-equal 23 h)
      (test-equal 1 m)
      (test-equal 2 s)))

;; moment

(test-group "Moment constructors, getters"
    (test-assert (moment? (make-moment (make-date 2025 1 1) 100)))
    (test-error (make-moment #f 100))
    (test-error (make-moment (make-date 2025 1 1) -1))
    (test-error (make-moment (make-date 2025 1 1) 86401))

    (test-assert (date=? (make-date 2025 1 1) (moment-date (make-moment (make-date 2025 1 1) 1))))
    (test-equal 1 (moment-second-of-day (make-moment (make-date 2025 1 1) 1))))

(test-group "Moment comparators"
    (test-assert (not (moment=? (make-moment (make-date 2024 1 1) 1) (make-moment (make-date 2025 1 1) 1))))
    (test-assert (not (moment=? (make-moment (make-date 2025 1 1) 2) (make-moment (make-date 2025 1 1) 1))))
    (test-assert (moment=? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2025 1 1) 1)))

    (test-assert (moment<? (make-moment (make-date 2024 1 1) 2) (make-moment (make-date 2025 1 1) 1)))
    (test-assert (moment<? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2025 1 1) 2)))
    (test-assert (not (moment<? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2025 1 1) 1))))
    (test-assert (not (moment<? (make-moment (make-date 2026 1 1) 1) (make-moment (make-date 2025 1 1) 1))))
    (test-assert (not (moment<? (make-moment (make-date 2025 1 1) 2) (make-moment (make-date 2025 1 1) 1))))

    (test-assert (moment<=? (make-moment (make-date 2024 1 1) 2) (make-moment (make-date 2025 1 1) 1)))
    (test-assert (moment<=? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2025 1 1) 2)))
    (test-assert (moment<=? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2025 1 1) 1)))
    (test-assert (not (moment<=? (make-moment (make-date 2026 1 1) 1) (make-moment (make-date 2025 1 1) 1))))
    (test-assert (not (moment<=? (make-moment (make-date 2025 1 1) 2) (make-moment (make-date 2025 1 1) 1))))

    (test-assert (moment>? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2024 1 1) 2)))
    (test-assert (moment>? (make-moment (make-date 2025 1 1) 2) (make-moment (make-date 2025 1 1) 1)))
    (test-assert (not (moment>? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2025 1 1) 1))))
    (test-assert (not (moment>? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2026 1 1) 1))))
    (test-assert (not (moment>? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2025 1 1) 2))))

    (test-assert (moment>=? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2024 1 1) 2)))
    (test-assert (moment>=? (make-moment (make-date 2025 1 1) 2) (make-moment (make-date 2025 1 1) 1)))
    (test-assert (moment>=? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2025 1 1) 1)))
    (test-assert (not (moment>=? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2026 1 1) 1))))
    (test-assert (not (moment>=? (make-moment (make-date 2025 1 1) 1) (make-moment (make-date 2025 1 1) 2)))))

(test-group "Timestamp constructors, getters"
    (test-assert (timestamp? (make-timestamp 2020 1 1 10 0 0 (tz-timezone "Europe/Vilnius"))))
    (test-assert (timestamp? (make-timestamp 2020 1 1 10 0 0 (tz-timezone "Europe/Vilnius") 0)))
    (test-assert (timestamp? (date+clock-time->timestamp (make-date 2020 1 1) (make-clock-time 10 0 0) (tz-timezone "Europe/Vilnius"))))
    (test-assert (timestamp? (date+clock-time->timestamp (make-date 2020 1 1) (make-clock-time 10 0 0) (tz-timezone "Europe/Vilnius") 0)))
    (test-error (make-timestamp 2020 1 1 10 0 0 (tz-timezone "Europe/Vilnius") 1))

    ;; clock is moved forward at 3:00, such time doesn't exist
    (test-error (make-timestamp 2025 3 30 3 30 0 (tz-timezone "Europe/Vilnius")))
    ;; clock is moved backwards at 4:00, this time happens twice, check if fold = 1 works
    (test-assert (timestamp? (make-timestamp 2025 10 26 3 30 0 (tz-timezone "Europe/Vilnius") 0)))
    (test-assert (timestamp? (make-timestamp 2025 10 26 3 30 0 (tz-timezone "Europe/Vilnius") 1)))

    ;; invalid leapsecond
    (test-error (make-timestamp 2015 7 1 2 58 60 (tz-timezone "Europe/Vilnius")))
    ;; valid leapsecond
    (test-assert (timestamp? (make-timestamp 2015 7 1 2 59 60 (tz-timezone "Europe/Vilnius"))))
    ;; leapsecond with fraction
    (test-assert (timestamp? (make-timestamp 2015 7 1 2 59 (+ 60 1/2) (tz-timezone "Europe/Vilnius"))))

    (test-assert (date=? (make-date 2020 1 1) (timestamp-date (make-timestamp 2020 1 1 10 0 0 (tz-timezone "Europe/Vilnius")))))
    (test-equal 2020 (timestamp-year (make-timestamp 2020 1 1 10 0 0 (tz-timezone "Europe/Vilnius"))))
    (test-equal 1 (timestamp-month (make-timestamp 2020 1 2 10 0 0 (tz-timezone "Europe/Vilnius"))))
    (test-equal 2 (timestamp-day (make-timestamp 2020 1 2 10 0 0 (tz-timezone "Europe/Vilnius"))))
    (let-values (((y m d) (timestamp-ymd (make-timestamp 2020 1 2 10 0 0 (tz-timezone "Europe/Vilnius")))))
        (test-equal 2020 y)
        (test-equal 1 m)
        (test-equal 2 d))

    (test-equal 10 (clock-time-hour (timestamp-clock-time (make-timestamp 2020 1 1 10 1 2 (tz-timezone "Europe/Vilnius")))))
    (test-equal 1 (clock-time-minute (timestamp-clock-time (make-timestamp 2020 1 1 10 1 2 (tz-timezone "Europe/Vilnius")))))
    (test-equal 2 (clock-time-second (timestamp-clock-time (make-timestamp 2020 1 1 10 1 2 (tz-timezone "Europe/Vilnius")))))
    (test-equal 10 (timestamp-hour (make-timestamp 2020 1 1 10 1 2 (tz-timezone "Europe/Vilnius"))))
    (test-equal 1 (timestamp-minute (make-timestamp 2020 1 1 10 1 2 (tz-timezone "Europe/Vilnius"))))
    (test-equal 2 (timestamp-second (make-timestamp 2020 1 1 10 1 2 (tz-timezone "Europe/Vilnius"))))
    (let-values (((h m s) (timestamp-hms (make-timestamp 2020 1 1 10 1 2 (tz-timezone "Europe/Vilnius")))))
        (test-equal 10 h)
        (test-equal 1 m)
        (test-equal 2 s))

    (test-equal (tz-timezone "Europe/Vilnius") (timestamp-timezone (make-timestamp 2020 1 1 10 1 2 (tz-timezone "Europe/Vilnius"))))
    (test-equal 0 (timestamp-fold (make-timestamp 2025 10 26 3 30 0 (tz-timezone "Europe/Vilnius"))))
    (test-equal 1 (timestamp-fold (make-timestamp 2025 10 26 3 30 0 (tz-timezone "Europe/Vilnius") 1))))

(test-group "Moment to/from timestamp conversion"
    ;; non-leap case
    (let ((moment (make-moment (make-date 2026 2 3) (+ (* 1 3600) (* 2 60) 3 37)))
          (timestamp (make-timestamp 2026 2 3 3 2 3 (tz-timezone "Europe/Vilnius"))))
      (test-assert (moment=? moment (timestamp->moment timestamp)))
      (test-assert (timestamp-equal? timestamp (moment->timestamp moment (tz-timezone "Europe/Vilnius")))))

    ;; leap case
    (let ((moment (make-moment (make-date 2015 7 1) (+ 35)))
          (timestamp (make-timestamp 2015 7 1 2 59 (+ 60) (tz-timezone "Europe/Vilnius"))))
      (test-assert "moment leap =" (moment=? moment (timestamp->moment timestamp)))
      (test-assert "timestamp leap =" (timestamp-equal? timestamp (moment->timestamp moment (tz-timezone "Europe/Vilnius"))))))

(test-group "Timestamp to/from posix"
    (let ((t1 (make-timestamp 2015 6 30 23 59 59 (utc-timezone)))
          (t2 (make-timestamp 2015 6 30 23 59 60 (utc-timezone)))
          (t3 (make-timestamp 2015 7 1 2 59 59 (tz-timezone "Europe/Vilnius")))
          (posix 1435708799))
      (test-equal "t1" posix (timestamp->utc-posix-time t1))
      (test-equal "t2" posix (timestamp->utc-posix-time t2))
      (test-equal "t3" posix (timestamp->utc-posix-time t3))
      (test-assert "t1 and posix" (timestamp-equal? t1 (posix-time->utc-timestamp posix)))))

(test-group "Timestamp timezone conversion"
    (let ((t1 (make-timestamp 2026 2 3 5 0 0 (tz-timezone "Europe/Vilnius")))
          (t2 (make-timestamp 2026 2 2 22 0 0 (tz-timezone "America/New_York"))))
      (test-assert (timestamp-equal? t1 (timestamp-in-timezone t2 (tz-timezone "Europe/Vilnius"))))
      (test-assert (timestamp-equal? t2 (timestamp-in-timezone t1 (tz-timezone "America/New_York"))))))

(test-group "Timestamp timezone offset"
    (let ((t1 (make-timestamp 2026 2 3 5 0 0 (tz-timezone "Europe/Vilnius")))
          (dt1 (hours-delta 2))
          (t2 (make-timestamp 2026 2 2 22 0 0 (tz-timezone "America/New_York")))
          (dt2 (hours-delta -5)))
      (test-assert (dt-equal? dt1 (timestamp-timezone-offset t1)))
      (test-assert (dt-equal? dt2 (timestamp-timezone-offset t2)))))

(test-group "Timestamp dst? and abbreviation"
  (let ((dst (make-timestamp 2026 03 08 12 0 0 (tz-timezone "America/New_York")))
        (std (make-timestamp 2026 03 08 1 0 0 (tz-timezone "America/New_York"))))
    (test-assert "dst?" (timestamp-timezone-dst? dst))
    (test-equal "is EDT" "EDT" (timestamp-timezone-abbreviation dst))
    (test-assert "not dst?" (not (timestamp-timezone-dst? std)))
    (test-equal "is EST" "EST" (timestamp-timezone-abbreviation std))))

(test-group "Timestamp->ISO-8601"
    (test-equal "2026-01-02T03:04:05+02:00" (timestamp->iso-8601 (make-timestamp 2026 1 2 3 4 5 (tz-timezone "Europe/Vilnius"))))
    (test-equal "2026-01-02T03:04:05-05:00" (timestamp->iso-8601 (make-timestamp 2026 1 2 3 4 5 (tz-timezone "America/New_York"))))
    (test-equal "2026-01-02T03:04:05Z" (timestamp->iso-8601 (make-timestamp 2026 1 2 3 4 5 (utc-timezone))))
    (test-equal "2026-01-02T03:04:05+02:30" (timestamp->iso-8601 (make-timestamp 2026 1 2 3 4 5 (utc-offset-timezone (time-delta+ (hours-delta 2) (minutes-delta 30))))))
    (test-equal "2026-01-02T03:04:05.1Z" (timestamp->iso-8601 (make-timestamp 2026 1 2 3 4 (+ 5 1/10) (utc-timezone))))
    (test-equal "2026-01-02T03:04:05.333333333Z" (timestamp->iso-8601 (make-timestamp 2026 1 2 3 4 (+ 5 1/3) (utc-timezone)))))

;; timezone

(test-group "Timezones"
    (test-assert (timezone? (utc-timezone)))
    (test-assert (timezone? (utc-offset-timezone (hours-delta 1))))
    (test-assert (timezone? (system-timezone)))
    (test-assert (list? (tz-timezones)))
    (test-assert (timezone? (tz-timezone (list-ref (tz-timezones) 0)))))

;; dt

(test-group "dt"
  (let* ((time-delta (time-delta+ (years-delta 1)
                                  (years-delta 10)
                                  (months-delta 2)
                                  (months-delta 10)
                                  (weeks-delta 3)
                                  (weeks-delta 10)
                                  (days-delta 4)
                                  (days-delta 10)
                                  (hours-delta 5)
                                  (hours-delta 10)
                                  (minutes-delta 6)
                                  (minutes-delta 10)
                                  (seconds-delta 7)
                                  (seconds-delta 10)))
         (time-delta* (time-delta-negate time-delta)))
    (test-assert (time-delta? time-delta))
    (test-equal 11 (time-delta-years time-delta))
    (test-equal 12 (time-delta-months time-delta))
    (test-equal 13 (time-delta-weeks time-delta))
    (test-equal 14 (time-delta-days time-delta))
    (test-equal 15 (time-delta-hours time-delta))
    (test-equal 16 (time-delta-minutes time-delta))
    (test-equal 17 (time-delta-seconds time-delta))
    (test-equal -11 (time-delta-years time-delta*))
    (test-equal -12 (time-delta-months time-delta*))
    (test-equal -13 (time-delta-weeks time-delta*))
    (test-equal -14 (time-delta-days time-delta*))
    (test-equal -15 (time-delta-hours time-delta*))
    (test-equal -16 (time-delta-minutes time-delta*))
    (test-equal -17 (time-delta-seconds time-delta*))))

;; date arithmetic

(test-group "date+"
    (test-assert (date=? (make-date 2021 12 31) (date+ (make-date 2021 1 1) (time-delta+ (days-delta -1) (years-delta 1)))))
    (test-assert (date=? (make-date -2020 1 1) (date+ (make-date -2021 1 1) (years-delta 1))))
    (test-assert (date=? (make-date 2021 2 28) (date+ (make-date 2021 1 31) (months-delta 1))))
    (test-assert (date=? (make-date 2021 1 8) (date+ (make-date 2021 1 1) (weeks-delta 1))))
    ;; months overflow
    (test-assert (date=? (make-date 2022 2 1) (date+ (make-date 2021 1 1) (months-delta 13))))
    ;; days overflow
    (test-assert (date=? (make-date 2022 2 1) (date+ (make-date 2021 1 1) (days-delta 396))))
    (test-error (date+ (make-date 2021 1 1) (hours-delta 1)))
    (test-error (date+ (make-date 2021 1 1) (minutes-delta 1)))
    (test-error (date+ (make-date 2021 1 1) (seconds-delta 1))))

(test-group "timestamp+"
    ;; recheck date+ cases
    (test-assert (timestamp-equal? (make-timestamp 2021 12 31 0 0 0 (utc-timezone)) (timestamp+ (make-timestamp 2021 1 1 0 0 0 (utc-timezone)) (time-delta+ (days-delta -1) (years-delta 1)))))
    (test-assert (timestamp-equal? (make-timestamp -2020 1 1 0 0 0 (utc-timezone)) (timestamp+ (make-timestamp -2021 1 1 0 0 0 (utc-timezone)) (years-delta 1))))
    (test-assert (timestamp-equal? (make-timestamp 2021 2 28 0 0 0 (utc-timezone)) (timestamp+ (make-timestamp 2021 1 31 0 0 0 (utc-timezone)) (months-delta 1))))
    (test-assert (timestamp-equal? (make-timestamp 2021 1 8 0 0 0 (utc-timezone)) (timestamp+ (make-timestamp 2021 1 1 0 0 0 (utc-timezone)) (weeks-delta 1))))
    ;; months overflow
    (test-assert (timestamp-equal? (make-timestamp 2022 2 1 0 0 0 (utc-timezone)) (timestamp+ (make-timestamp 2021 1 1 0 0 0 (utc-timezone)) (months-delta 13))))
    ;; days overflow
    (test-assert (timestamp-equal? (make-timestamp 2022 2 1 0 0 0 (utc-timezone)) (timestamp+ (make-timestamp 2021 1 1 0 0 0 (utc-timezone)) (days-delta 396))))

    (test-assert (timestamp-equal? (make-timestamp 2021 1 1 1 0 0 (utc-timezone)) (timestamp+ (make-timestamp 2021 1 1 0 0 0 (utc-timezone)) (hours-delta 1))))
    (test-assert (timestamp-equal? (make-timestamp 2020 12 31 23 0 0 (utc-timezone)) (timestamp+ (make-timestamp 2021 1 1 0 0 0 (utc-timezone)) (hours-delta -1))))
    (test-assert (timestamp-equal? (make-timestamp 2021 1 2 6 0 0 (utc-timezone)) (timestamp+ (make-timestamp 2021 1 1 0 0 0 (utc-timezone)) (hours-delta 30))))
    (test-assert (timestamp-equal? (make-timestamp 2021 1 1 0 1 0 (utc-timezone)) (timestamp+ (make-timestamp 2021 1 1 0 0 0 (utc-timezone)) (minutes-delta 1))))
    (test-assert (timestamp-equal? (make-timestamp 2020 12 31 23 59 0 (utc-timezone)) (timestamp+ (make-timestamp 2021 1 1 0 0 0 (utc-timezone)) (minutes-delta -1))))
    (test-assert (timestamp-equal? (make-timestamp 2021 1 1 10 0 0 (utc-timezone)) (timestamp+ (make-timestamp 2021 1 1 0 0 0 (utc-timezone)) (minutes-delta 600))))
    (test-assert (timestamp-equal? (make-timestamp 2021 1 1 0 0 1 (utc-timezone)) (timestamp+ (make-timestamp 2021 1 1 0 0 0 (utc-timezone)) (seconds-delta 1))))
    (test-assert (timestamp-equal? (make-timestamp 2020 12 31 23 59 59 (utc-timezone)) (timestamp+ (make-timestamp 2021 1 1 0 0 0 (utc-timezone)) (seconds-delta -1))))
    (test-assert (timestamp-equal? (make-timestamp 2021 1 1 0 10 0 (utc-timezone)) (timestamp+ (make-timestamp 2021 1 1 0 0 0 (utc-timezone)) (seconds-delta 600))))

    ;; fractional seconds addition
    (test-assert (timestamp-equal? (make-timestamp 2021 1 1 0 0 (+ 2 3/4) (utc-timezone)) (timestamp+ (make-timestamp 2021 1 1 0 0 (+ 1 1/4) (utc-timezone)) (seconds-delta (+ 1 1/2)))))
    ;; adding hour during transition to DST should skip an hour in local time
    (test-assert (timestamp-equal? (make-timestamp 2025 3 30 4 30 0 (tz-timezone "Europe/Vilnius")) (timestamp+ (make-timestamp 2025 3 30 2 30 0 (tz-timezone "Europe/Vilnius")) (hours-delta 1))))
    (test-assert (timestamp-equal? (make-timestamp 2025 3 30 2 30 0 (tz-timezone "Europe/Vilnius")) (timestamp+ (make-timestamp 2025 3 30 4 30 0 (tz-timezone "Europe/Vilnius")) (hours-delta -1))))
    ;; adding hour during transition from DST should keep same local time but set fold flag
    (test-assert (timestamp-equal? (make-timestamp 2025 10 26 3 30 0 (tz-timezone "Europe/Vilnius") 1) (timestamp+ (make-timestamp 2025 10 26 3 30 0 (tz-timezone "Europe/Vilnius")) (hours-delta 1))))
    (test-assert (timestamp-equal? (make-timestamp 2025 10 26 3 30 0 (tz-timezone "Europe/Vilnius")) (timestamp+ (make-timestamp 2025 10 26 3 30 0 (tz-timezone "Europe/Vilnius") 1) (hours-delta -1))))
    ;; check fold flag is cleared when moving past the duplicate region
    (test-assert (timestamp-equal? (make-timestamp 2025 10 26 4 30 0 (tz-timezone "Europe/Vilnius")) (timestamp+ (make-timestamp 2025 10 26 3 30 0 (tz-timezone "Europe/Vilnius") 1) (hours-delta 1))))
    (test-assert (timestamp-equal? (make-timestamp 2025 10 26 3 30 0 (tz-timezone "Europe/Vilnius") 1) (timestamp+ (make-timestamp 2025 10 26 4 30 0 (tz-timezone "Europe/Vilnius")) (hours-delta -1))))
    ;; check using seconds-delta correctly accounts for leap seconds
    (test-assert (timestamp-equal? (make-timestamp 2015 7 1 3 0 2 (tz-timezone "Europe/Vilnius")) (timestamp+ (make-timestamp 2015 7 1 2 59 58 (tz-timezone "Europe/Vilnius")) (seconds-delta 5))))
    (test-assert (timestamp-equal? (make-timestamp 2015 7 1 2 59 58 (tz-timezone "Europe/Vilnius")) (timestamp+ (make-timestamp 2015 7 1 3 0 2 (tz-timezone "Europe/Vilnius")) (seconds-delta -5))))
    ;; check when using higher denomination when adding to timestamp during leap second, that second value is correctly adjusted back to 59
    (test-assert (timestamp-equal? (make-timestamp 2015 7 1 3 59 59 (tz-timezone "Europe/Vilnius")) (timestamp+ (make-timestamp 2015 7 1 2 59 60 (tz-timezone "Europe/Vilnius")) (hours-delta 1))))
    ;; regression test; date addition for old dates in timezone
    (test-assert (timestamp-equal? (make-timestamp -2020 1 1 0 0 0 (tz-timezone "Europe/Vilnius")) (timestamp+ (make-timestamp -2021 1 1 0 0 0 (tz-timezone "Europe/Vilnius")) (years-delta 1)))))

;; misc

(test-assert "current-moment" (moment? (current-moment)))
(test-assert "current-utc-timestamp" (timestamp? (current-utc-timestamp)))
(test-assert "current-system-timestamp" (timestamp? (current-system-timestamp)))

(test-end)
