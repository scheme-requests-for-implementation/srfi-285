;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define (discover-tzdb base-directory)
  (define names '())
  (define (discover! directory base)
    (for-each (lambda (filename)
                (when (not (member filename '("." "..")))
                  (let ((path (string-append directory "/" filename)))
                    (cond
                      ((filename-directory? path)
                       (discover! path
                                  (string-append base filename "/")))
                      ((tzif-file? path)
                       (set! names (cons (cons (string-append base filename)
                                               path)
                                         names)))))))
              (directory-files directory)))
  (discover! base-directory "")
  names)

(define discover-tzpath
  (case-lambda
    (()
     (let ((path (get-environment-variable "TZPATH")))
       (discover-tzpath (or path "/usr/share/zoneinfo"))))
    ((path)
     (unless (string? path)
       (error "not a string" path))
     (append-map discover-tzdb (string-split path ":")))))

(define (discover-leap-seconds* directory-name)
  (let ((files (directory-files directory-name)))
    (cond
      ((member "leap-seconds.list" files)
       => (lambda (pair)
            (list (string-append directory-name
                                 "/leap-seconds.list"))))
      (else '()))))

(define discover-leap-seconds
  (case-lambda
    (()
     (let ((path (get-environment-variable "TZPATH")))
       (discover-leap-seconds (or path "/usr/share/zoneinfo"))))
    ((path)
     (unless (string? path)
       (error "not a string" path))
     (append-map discover-leap-seconds* (string-split path ":")))))