#lang typed/racket

(define-struct None () #:transparent)
(define-struct (A) Some ([v : A]) #:transparent)
(provide (struct-out None) (struct-out Some))
(define-type (Opt A) (U None (Some A)))
(provide Opt)

(: snd (All (A B) (-> (Pair A B) B)))
(define (snd p) (cdr p))
(provide snd)