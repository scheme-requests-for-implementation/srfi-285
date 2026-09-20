;;; SPDX-FileCopyrightText: 2026 Peter McGoron
;;; SPDX-License-Identifier: MIT

(define-library (tzif-parser local-time)
  (import (scheme base) (tzif-parser tzif))
  (export time-range time-range?
          time-range-start time-range-end time-range-offset
          time-range-dst? time-range-designation
          time-range->datum
          range/as-is range/local
          search-ranges)
  (include "local-time.scm"))