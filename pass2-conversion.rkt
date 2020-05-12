#lang typed/racket

(require "./pass1-ast.rkt")
(require "./pass2-ast.rkt")
(require "./common.rkt")



;; Environment related

(define-type UnitLookup (Pair String (Opt P2-Import-Path)))
(define-type Environment (Listof UnitLookup))
(define-type Aliases (Listof (Pair String P2-Import-Path)))

(define/match (to-alias import)
  [((P1-Import names path alias))
   (cons alias (p2-convert-import-path path))])

(define/match (to-env import)
  [((P1-Import names path alias))
   (for/list : (Listof (Pair String (Opt P2-Import-Path))) ([n names])
     (cons n (Some (p2-convert-import-path path))))])

(: lookup (-> String P2-Import-Path UnitLookup))
(define (lookup name path)
  (cons name (Some path)))

(: local-var (-> String UnitLookup))
(define (local-var name)
  (cons name (None)))

(: add-local-var (-> String Environment Environment))
(define (add-local-var name env)
  (cons (cons name (None)) env))

(: add-lookup (-> String P2-Import-Path Environment Environment))
(define (add-lookup name path env)
  (cons (cons name (Some path)) env))

(define prim-term-env : Environment
  (list (local-var "add-i32")
        (local-var "sub-i32")
        (local-var "mul-i32")
        (local-var "div-i32")
        (local-var "True")
        (local-var "False")))

(define prim-type-env : Environment
  (list (local-var "I32")
        (local-var "Bool")
        (local-var "Char")
        (local-var "-->")
        (local-var "Tuple")
        (local-var "List")
        (local-var "Vector")
        (local-var "Slice")
        (local-var "Dict")
        (local-var "Record")
        (local-var "Variant")
        (local-var "Bag")
        (local-var "Union")
        (local-var "Ref")))

(define term-env : (Parameterof Environment) (make-parameter prim-term-env))

(define type-env : (Parameterof Environment) (make-parameter prim-type-env))

(define aliases : (Parameterof Aliases) (make-parameter null))



(: pass2-convert (-> P1-Boba-Program P2-Boba-Program))
(define (pass2-convert program)
  (define main (p2-main (car program)))
  (define units
    (for/hash : P2-Boba-Units ([(k v) (in-hash (cdr program))])
      (values (p2-convert-import-path k) (p2-unit v))))
  (displayln "Warning: imported type names are currently not checked for validation.")
  ((inst cons P2-Boba-Main P2-Boba-Units) main units))

(: p2-convert-import-path (-> P1-Import-Path P2-Import-Path))
(define/match (p2-convert-import-path path)
  [((P1-Remote-Path org proj name major minor patch))
   (string-append org "." proj "." name ":" (number->string major) "." (number->string minor) "." (number->string patch))]
  [((? string? x)) x])

(: p2-main (-> P1-Boba-Main P1-Boba-Units P2-Boba-Main))
(define (p2-main main units)
  (match main
    [(P1-Boba-Main imports decls body)
     (parameterize ([aliases (map P1-Import-alias imports)]
                    [term-env (append* (get-imported-terms units imports) term-env)]
                    [type-env (append* (get-imported-types units imports) type-env)])
       (define
         (P2-Boba-Main
          (transform-decls decls)
          (p2-expr body aliases types terms))))]))

(: p2-unit (-> P1-Boba-Unit P2-Boba-Unit))
(define (p2-unit boba-unit units)
  (match boba-unit
    [(P1-Boba-Unit imports decls export)
     (parameterize ([aliases (map P1-Import-alias imports)]
                    [term-env (append* (get-imported-terms units imports) term-env)]
                    [type-env (append* (get-imported-types units imports) type-env)])
       (transform-decls decls))]))

(: get-type-names (-> P1-Boba-Units (Listof P1-Import) Environment))
(define (get-type-names units imports)
  (for/list ([i imports])
    (define referenced (hash-ref (P1-Import-path i)))
    (define (is-type-name-in-unit name)
      #f)
    (for/list ([n (P1-Import-names i)]
               #:when (is-type-name-in n))
      n)))

(: get-term-names (-> P1-Boba-Units (Listof P1-Import) Environment))
(define (get-term-names units imports)
  (for/list ([i imports])
    (define referenced (hash-ref (P1-Import-path i)))
    (define (is-term-name-in-unit name)
      #t)
    (for/list ([n (P1-Import-names i)]
               #:when (is-term-name-in n))
      n)))

(: transform-decls (-> (Listof P1-Declaration) Aliases Environment Environment P2-Boba-Unit))
(define (transform-decls decls aliases type-env term-env)
  (for/fold ([env : Environment start-env]
             [transformed : (Listof P2-Declaration) null]
             #:result transformed)
            ([d decls])
    (let-values ([(e2 d2) (p2-decl d aliases env)])
      (values e2 (cons d2 transformed)))))



(: p2-decl (-> P1-Declaration Aliases Environment (Values Environment P2-Declaration)))
(define (p2-decl decl aliases env)
  (match decl
    [(P1-Function name fixed body)
     (values
      (add-local-var name env)
      (P2-Function name fixed (p2-expr body aliases env)))]

    [_ (error "Pass 2 transform for most declarations currently unimplemented.")]))

(: p2-data-ctor (-> P1-Constructor Aliases Environment P2-Constructor))
(define (p2-data-ctor ctor aliases env)
  (match ctor
    [(P1-Constructor name elems)
     (P2-Constructor name
                     (for/list ([t elems]) (p2-type t aliases env)))]))



(: p2-type (-> P1-Type Aliases Environment P2-Type))
(define (p2-type type aliases env)
  (match type
    [(P1-Type-App left right)
     (P2-Type-App (p2-type left aliases env) (p2-type right aliases env))]
    [_ (error "Pass 2 transform for most types currently unimplemented")]))



(: p2-expr (-> (Listof P1-Word) Aliases Environment (Listof P2-Word)))
(define (p2-expr expr aliases env) (for/list ([w expr]) (p2-word w aliases env)))

(: p2-word (-> P1-Word Aliases Environment P2-Word))
(define (p2-word word aliases env) word)