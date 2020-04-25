#lang racket/base

(require "pass0-loading.rkt")
(require "pass1-import-export-check.rkt")
(require "pass2-adhoc-conditions.rkt")
(require "pass3-simplify-user-types.rkt")

(define (compile entry-file)
  (define pass0-results (pass0-loading entry-file))
  (displayln (string-append "Pass 0 completed successfully, loaded " (number->string (add1 (hash-count (cdr pass0-results)))) " boba units."))
  (define pass1-results (pass1-import-export-check pass0-results))
  (displayln (string-append "Pass 1 verified listed exported and imported names are valid."))
  (define pass2-results (pass2-adhoc-conditions pass1-results))
  (displayln (string-append "Pass 2 verified additional explicit predicate conditions."))
  (define pass3-results (pass3-simplify-user-types pass2-results))
  (displayln (string-append "Pass 3 successfully converted user-written types to simpler internal form.")))

(define (spit file)
  (displayln (load-boba-unit file)))

(provide compile)
(provide spit)