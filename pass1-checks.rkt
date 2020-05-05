#lang typed/racket

(require "./pass1-ast.rkt")



(: check-exports (-> P1-Boba-Program Void))
(define (check-exports program)
  (for ([(k v) (in-hash (cdr program))])
    (check-unit-exports k v)))
(provide check-exports)

(: check-unit-exports (-> P1-Import-Path P1-Boba-Unit Void))
(define (check-unit-exports unit-name boba-unit)
  (check-exports-against-decls (get-decl-names boba-unit)
                               (P1-Boba-Unit-exports boba-unit))
  (warn-duplicate-exports unit-name (P1-Boba-Unit-exports boba-unit)))

(: id (-> String String))
(define (id x) x)

(: fst (-> (Listof String) String))
(define (fst x) (car x))

(: warn-duplicate-exports (-> P1-Import-Path (Listof String) Void))
(define (warn-duplicate-exports unit-name exports)
  (define grouped (group-by id exports))
  (define duplicated (map fst (filter (lambda ([x : (Listof String)]) (> 1 (length x))) grouped)))
  (when (not (null? duplicated))
    (displayln
     (string-append* "Unit '" (boba-path->string unit-name) "' has duplicate exports: " duplicated))))

(define (check-exports-against-decls declared exported) #t)

(define (get-decl-names boba-unit) null)