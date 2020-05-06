#lang typed/racket

(require "./pass1-ast.rkt")



(: id (All (A) (-> A A)))
(define (id x) x)

(: fst (All (A) (-> (Listof A) A)))
(define (fst x) (car x))

(: get-duplicates (All (A) (-> (Listof A) (Listof A))))
(define (get-duplicates ls)
  (map (inst fst A)
       (filter (lambda ([x : (Listof A)]) (not (null? (cdr x))))
               (group-by (inst id A) ls))))



;; Export checking

(: check-exports (-> P1-Boba-Program Void))
(define (check-exports program)
  (for ([(k v) (in-hash (cdr program))])
    (check-unit-exports k v)))
(provide check-exports)

(: check-unit-exports (-> P1-Import-Path P1-Boba-Unit Void))
(define (check-unit-exports unit-name boba-unit)
  (check-exports-against-decls unit-name
                               (get-decl-names boba-unit)
                               (list->set (P1-Boba-Unit-exports boba-unit)))
  (if (null? (P1-Boba-Unit-exports boba-unit))
      (displayln (string-append "Unit '" (boba-path->string unit-name) "' exports nothing."))
      (warn-duplicate-exports unit-name (P1-Boba-Unit-exports boba-unit))))

(: warn-duplicate-exports (-> P1-Import-Path (Listof String) Void))
(define (warn-duplicate-exports unit-name exports)
  (define duplicated (get-duplicates exports))
  (when (not (null? duplicated))
    (displayln
     (string-append* (string-append "Unit '" (boba-path->string unit-name) "' has duplicate exports: ") duplicated))))

(: check-exports-against-decls (-> P1-Import-Path (Setof String) (Setof String) Void))
(define (check-exports-against-decls unit-name declared exported)
  (define not-declared (set-subtract exported declared))
  (when (not (set-empty? not-declared))
    (error
     (string-append* (string-append "Unit '" (boba-path->string unit-name) "' does not declare exported names: ")
                     (set->list not-declared)))))

(: get-decl-names (-> P1-Boba-Unit (Setof String)))
(define (get-decl-names boba-unit)
  (list->set (append* (map get-decl-name (P1-Boba-Unit-declarations boba-unit)))))

(: get-decl-name (-> P1-Declaration (Listof String)))
(define (get-decl-name decl)
  (match decl
    [(P1-Data name _ _ _) (list name)]
    [(P1-Pattern-Synonym name _ _) (list name)]
    [(P1-Adhoc name predicate-name _ _) (list name predicate-name)]
    [(P1-Tag name tag) (list name tag)]
    [(P1-Type-Synonym name _ _) (list name)]
    [(P1-Effect-Synonym name _ _) (list name)]
    [(P1-Predicate-Synonym name _ _) (list name)]
    [(P1-Function name _ _) (list name)]
    [(P1-Recursive funcs) (append* (map get-decl-name funcs))]))



;; Import checking

(: check-imports (-> P1-Boba-Program Void))
(define (check-imports program)
  (check-unit-imports "main" (cdr program) (P1-Boba-Main-imports (car program)))
  (for ([(k v) (in-hash (cdr program))])
    (check-unit-imports k (cdr program) (P1-Boba-Unit-imports v))))
(provide check-imports)

(: check-unit-imports (-> P1-Import-Path P1-Boba-Units (Listof P1-Import) Void))
(define (check-unit-imports unit-name units imports)
  (for ([import imports])
    (check-explicit-imports-defined unit-name units import)
    (warn-duplicate-explicit-imports unit-name import))
  (warn-shadowed-explicit-imports unit-name imports)
  (warn-shadowed-import-alias unit-name imports))

(define (check-explicit-imports-defined unit-name units imports) (void))

(: warn-duplicate-explicit-imports (-> P1-Import-Path P1-Import Void))
(define (warn-duplicate-explicit-imports unit-name import)
  (define explicit-imports (P1-Import-names import))
  (define duplicated (get-duplicates explicit-imports))
  (when (not (null? duplicated))
    (displayln
     (string-append* (string-append "Unit '"
                                    (boba-path->string unit-name)
                                    "' has duplicate imports from '"
                                    (boba-path->string (P1-Import-path import))
                                    "': ")
                     duplicated))))

(define (warn-shadowed-explicit-imports unit-name imports) (void))

(: warn-shadowed-import-alias (-> P1-Import-Path (Listof P1-Import) Void))
(define (warn-shadowed-import-alias unit-name imports)
  (define duplicated (get-duplicates (map P1-Import-alias imports)))
  (when (not (null? duplicated))
    (displayln
     (string-append* (string-append "Unit '"
                                    (boba-path->string unit-name)
                                    "' defines shadowed unit aliases: ")
                     duplicated))))