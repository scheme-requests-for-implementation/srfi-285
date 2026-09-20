;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (tzif-parser modify-leap-seconds)
  (import (scheme base) (tzif-parser utils)
          (tzif-parser tzif) (tzif-parser leap-seconds))
  (export remove-leap-seconds add-leap-seconds)
  (include "modify-leap-seconds.scm"))