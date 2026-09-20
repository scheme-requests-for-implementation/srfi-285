;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (tzif-parser discover-tzdb)
  (export discover-tzdb discover-tzpath discover-leap-seconds)
  (import (scheme base) (scheme case-lambda) (scheme process-context)
          (tzif-parser tzif))
  (cond-expand
    ((library (srfi 170))
     (import (only (srfi 170)
                   file-info
                   file-info-directory?
                   directory-files))
     (begin
       (define (filename-directory? filename)
         (file-info-directory? (file-info filename #t)))))
    (chibi
     (import (rename (only (chibi filesystem)
                           file-directory?
                           directory-files)
                     (file-directory? filename-directory?))))
    (sagittarius
     (import (only (rename (sagittarius)
                           (file-directory? filename-directory?)
                           (read-directory directory-files))
                   filename-directory?
                   read-directory)))
    (else (begin (error "need a filesystem interface"))))
  (cond-expand
    ((library (srfi 152))
     (import (only (srfi 152) string-split)))
    ((library (srfi 130))
     (import (only (srfi 130) string-split)))
    (else
     (begin (error "need a string library"))))
  (cond-expand
    ((library (srfi 1))
     (import (only (srfi 1) append-map)))
    (else (error "need list library")))
  (include "discover-tzdb.scm"))