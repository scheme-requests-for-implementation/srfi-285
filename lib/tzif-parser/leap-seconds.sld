;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (tzif-parser leap-seconds)
  (import (scheme base) (scheme file) (scheme char))
  (export leap-seconds-info leap-seconds-info?
          leap-seconds-info-expiry
          leap-seconds-info-last-updated
          leap-seconds-info-occurences

          leap-second-occurence leap-second-occurence?
          leap-second-occurence-time leap-second-occurence-delta-tai

          leap-seconds-info->datum
          leap-second-occurence->datum

          read-leap-seconds
          delta-tai-for-timestamp)
  (include "leap-seconds.scm"))