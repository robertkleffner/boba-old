#lang racket/base

(require "pass0-loading.rkt")
(require "pass1-conversion.rkt")
(require "pass1-checks.rkt")
(require pprint)
;(require "pass2-conversion.rkt")

(define (compile to-phase entry-file)
  (let/ec return
    (define pass0-results (pass0-loading entry-file))
    (displayln (string-append "Pass 0 completed successfully, loaded " (number->string (add1 (hash-count (cdr pass0-results)))) " boba units."))
    (when (equal? to-phase "0")
      (return pass0-results))
    (define pass1-results (pass1-convert pass0-results))
    (displayln "Pass 1 conversion completed successfully.")
    (check-exports pass1-results)
    (check-imports pass1-results)
    (when (equal? to-phase "1")
      (return pass1-results))
    ;(define pass2-results (pass2-convert pass1-results))
    ;(displayln "Pass 2 conversion completed successfully.")
    ;pass2-results)
    ))

(define (spit file)
  (displayln (load-boba-unit file)))

(provide compile)
(provide spit)