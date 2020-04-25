#lang racket/base

(require racket/match)

(define (pass1-import-export-check program)
  (define main (car program))
  (define units (cdr program))
  (check-import-names-defined "main" main)
  (for ([(path u) (in-immutable-hash units)])
    (check-import-names-defined path u)
    (check-export-names-defined path u)
    (warn-export-empty path u))
  program)

(define (warn-export-empty path boba-unit)
  (match boba-unit
    [`(unit ,decls ... (export ,names ...))
     (when (null? names)
       (displayln "Warning - empty export clause in unit:")
       (displayln path))]))

(define (check-import-names-defined path boba-unit)
  (displayln "Warning - import name checking not yet implemented."))

(define (check-export-names-defined path boba-unit)
  (displayln "Warning - export name checking not yet implemented."))

(provide pass1-import-export-check)