#lang racket/base

(require "pass0-loading.rkt")

(define (compile entry-file)
  (define pass0-results (pass0-loading entry-file))
  (displayln (string-append "Pass 0 completed successfully, loaded " (number->string (hash-count pass0-results)) " boba units.")))

(define (spit file)
  (displayln (load-boba-unit file)))

(provide compile)
(provide spit)