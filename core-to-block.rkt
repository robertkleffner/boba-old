#lang algebraic/racket/base

(require racket/set)
(require racket/list)
(require "block-gen.rkt")
(module+ test
  (require rackunit))

; The primary purpose of this file is to translate a set of constructors and a Boba-core
; expression into flat, uniquely-named Boba-blocks. There are two key parts of this translation:
; 1) Renaming - all expression names are made into unique names, so that 'let shadowing' is
; no longer a potential problem. This also allows us to refer to each generated block with
; a unique label, simplifying some of the code generation for now.
; 2) Block extraction - a Boba-core expression contains many nested subexpressions, a good
; number of which are named 'let expressions'. We need to pull out all of these nested definitions
; to the top level, while respecting the truly captured 'free variables'. These obviously
; include captured value variables, but also captured function words that refer to known closures.
; Any named expression that is defined within a few key scenarios (basically within scope of a
; variable assignment) require this closure conversion step. This requires tracking of which names
; in the current scope refer to named closures and which named expressions don't need to be closures.



;; type Core-Program = (Ctors, Core-Expr)

;; type Core-Expr = [Core-Word]

(data Core-Word (C-Statement ; Core-Expr
                 C-Assign ; [ValVar], Core-Word
                 C-Let ; FuncVar, Core-Expr, Core-Word
                 C-LetRec ; [(FuncVar, Core-Expr)], Core-Word
                 C-Handle ; [ValVar], Core-Expr, [C-Operation], Core-Expr
                 C-Operation ; OpVar, [ValVar], Core-Expr
                 C-If ; Core-Word, Core-Word
                 C-While ; Core-Word, Core-Word
                 C-FuncVal ; Core-Expr
                 C-Vector
                 C-Slice
                 C-Dict
                 C-Record
                 C-Extension ; FieldVar
                 C-Restriction ; FieldVar
                 C-Selection ; FieldVar
                 C-Update ; FieldVar
                 C-Variant ; FieldVar
                 C-Embeding ; FieldVar
                 C-Case ; [(FieldVar, Core-Expr)], Core-Expr
                 C-Var ; FuncVar | ValVar | OpVar | ConVar | PredVar
                 C-Destruct
                 C-Number ; Number
                 ))



; Some of these are reserved words in the grammar, some are not
(define PRIMS
  (list "newref"
        "getref"
        "putref"

        "do"

        "add-i32"
        "sub-i32"
        "mul-i32"
        "div-i32"

        "bool-and"
        "bool-or"
        "bool-not"
        "bool-xor"

        "list-nil"
        "list-cons"
        "list-snoc"
        "list-head"
        "list-last"
        "list-tail"
        "list-init"
        "list-append"
        "list-empty"))

(define PRIM-RENAME (make-immutable-hash (for/list ([p PRIMS]) (cons p (string-append "$" p)))))

;; rename : Core-Word -> Core-Word
(define (rename word)

  ;; fresh-val-var : Var -> Var
  (define fresh-val-var!
    (let ([n 0])
      (lambda (val-var)
        (set! n (add1 n))
        (string-append val-var (number->string n)))))

  ;; fresh-func-var : Var -> Var
  (define fresh-func-var!
    (let ([n 0])
      (lambda (func-var)
        (set! n (add1 n))
        (string-append "$" func-var (number->string n)))))

  ;; rename-word: { Var -> Var }, Core-Word -> Core-Word
  (define (rename-word replace word)
    (define f
      (function
       [(C-Assign values wd)
        (define new-env
          (for/fold ([new-rep replace])
                    ([var values])
            (hash-set new-rep var (fresh-val-var! var))))
        (C-Assign (for/list ([var values]) (hash-ref new-env var))
                  (rename-word new-env wd))]
       [(C-Let f-var let-exp let-in)
        (define new-env (hash-set replace f-var (fresh-func-var! f-var)))
        (C-Let (hash-ref new-env f-var)
               (rename-expr replace let-exp)
               (rename-word new-env let-in))]
       [(C-LetRec var-exprs let-in)
        (define new-env
          (for/fold ([new-rep replace])
                    ([var (map car var-exprs)])
            (hash-set new-rep var (fresh-func-var! var))))
        (C-LetRec (for/list ([v-e var-exprs])
                    (cons (car v-e) (rename-expr new-env (cdr v-e))))
                  (rename-word new-env let-in))]
       [(C-Handle params handled ops return)
        (define new-env
          (for/fold ([new-rep replace])
                    ([var params])
            (hash-set new-rep var (fresh-val-var! var))))
        (C-Handle (for/list ([v params]) (hash-ref new-env v))
                  (rename-expr replace handled)
                  (for/list ([o ops]) (rename-word new-env o))
                  (rename-expr new-env return))]
       [(C-Operation op params op-expr)
        (define new-env
          (for/fold ([new-rep replace])
                    ([var params])
            (hash-set new-rep var (fresh-val-var! var))))
        (C-Operation op
                     (for/list ([v params]) (hash-ref new-env v))
                     (rename-expr new-env op-expr))]
       [(C-Statement expr)
        (C-Statement (rename-expr replace expr))]
       [(C-If then else)
        (C-If (rename-word replace then) (rename-word replace else))]
       [(C-While test body)
        (C-While (rename-word replace test) (rename-word replace body))]
       [(C-FunctionLit expr)
        (C-FunctionLit (rename-expr replace expr))]
       [(C-Case clauses otherwise)
        (C-Case
         (for/list ([c clauses])
           (cons (car c) (rename-expr replace (cdr c))))
         (rename-expr replace otherwise))]
       [(C-Var name)
        (C-Var (hash-ref replace name))]
       [a a]))
    (f word))

  ;; rename-expr: { Var -> Var }, Core-Expr -> Core-Expr
  (define (rename-expr replace expr)
    (for/list ([w expr])
      (rename-word replace w)))
  
  (rename-word PRIM-RENAME word))

(module+ test
  (check-equal? (C-Var "$do") (rename (C-Var "do")))
  (check-equal? (C-Assign (list "a1") (C-Var "a1")) (rename (C-Assign (list "a") (C-Var "a"))))
  (check-equal? (C-Assign (list "a1" "b2") (C-Assign (list "a3") (C-Var "a3")))
                (rename (C-Assign (list "a" "b") (C-Assign (list "a") (C-Var "a"))))))



(data ClosedKind (Free Closure Val))

;; type ClosureEnv = { FuncVar|ValVar -> ClosedKind }

;; captured? : ClosedKind -> Bool
(define captured?
  (function
   [(Free) #false]
   [(Closure) #true]
   [(Val) #true]))

;; captured-vars : ClosureEnv, Core-Word -> [FuncVar|ValVar]
(define (captured-vars env word)
  (set->list (captured-vars-set env word)))

;; captured-vars-set-expr : ClosureEnv, [Core-Word] -> Set (FuncVar|ValVar)
(define (captured-vars-set-expr env expr)
  (apply set-union (for/list ([w expr]) (captured-vars-set env w))))

;; captured-vars-set : ClosureEnv, Core-Word -> Set (FuncVar|ValVar)
;; TODO: using the constraint of global unique names from the renaming step,
;; it should be possible to simplify this function, particularly no longer
;; needing to account for scope.
(define (captured-vars-set env word)
  (define f
    (function
     [(C-Statement expr)
      (captured-vars-set-expr env expr)]
     [(C-Assign values scope-wd)
      (set-subtract (captured-vars-set env scope-wd) (list->set values))]
     [(C-Let f-var let-exp let-in)
      (set-union (captured-vars-set-expr env let-exp)
                 (set-remove (captured-vars-set env let-in) f-var))]
     [(C-LetRec var-exprs let-in)
      (set-subtract (set-union (captured-vars-set env let-in)
                               (apply set-union (for/list ([v-e var-exprs]) (captured-vars-set-expr env (cdr v-e)))))
                    (list->set (map first var-exprs)))]
     [(C-Handle params handled ops return)
      (set-union (captured-vars-set-expr env handled)
                 (set-subtract (captured-vars-set-expr env return) (list->set params))
                 (set-subtract (apply set-union (for/list ([op ops]) (captured-vars-set env op))) (list->set params)))]
     [(C-Operation op params expr)
      (set-subtract (captured-vars-set env expr) (list->set params))]
     [(C-If then else)
      (set-union (captured-vars-set env then) (captured-vars-set env else))]
     [(C-While test body)
      (set-union (captured-vars-set env test) (captured-vars-set env body))]
     [(C-FuncVal expr)
      (captured-vars-set-expr env expr)]
     [(C-Case clauses otherwise)
      (set-union (captured-vars-set-expr env otherwise)
                 (apply set-union (for/list ([c clauses]) (captured-vars-set-expr env (cdr c)))))]
     [(C-Var name)
      (if (hash-has-key? env name)
          (if (captured? (hash-ref env name))
              (set name)
              (set))
          (error "Name not found"))]
     [a (set)]))
  (f word))

(module+ test
  (check-equal? (captured-vars (hash "a" (Free)) (C-Var "a")) (list))
  (check-equal? (captured-vars (hash "a" (Val)) (C-Var "a")) (list "a"))
  (check-equal? (captured-vars (hash "a" (Closure)) (C-Var "a")) (list "a"))

  (check-equal? (captured-vars (hash "a" (Val) "b" (Val))
                               (C-Assign (list "a") (C-Statement (list (C-Var "a") (C-Var "b")))))
                (list "b"))
  (check-equal? (captured-vars (hash "a" (Val) "b" (Val))
                               (C-Let "a" (list (C-Var "b")) (C-Var "a")))
                (list "b")))



(define (add-params vars env)
  (for/fold ([new-env env])
            ([v vars])
    (hash-set new-env v Val)))

(define (add-values vars env)
  (for/fold ([new-env env])
            ([v vars])
    (hash-set new-env v Val)))

(define (add-let f-var blocks env)
  (define captured ((φ (B-Func name free wd) free) (hash-ref blocks f-var)))
  (define is-closure (ormap captured? (for/list ([c captured]) (hash-ref env c))))
  (hash-set env f-var (if is-closure (Closure) (Free))))

(define (add-let-rec mutuals free env)
  (for/fold ([new-env env])
            ([m mutuals])
    (hash-set new-env m free)))

;; extract-blocks: Core-Word -> { FuncVar -> BlockDef }
;; This function is a wrapper for a stateful building-up of a block hash table.
(define (extract-blocks word)
  (define blocks (make-hash))

  ;; fresh-func-var : Var -> Var
  (define fresh-func-var!
    (let ([n 0])
      (lambda (func-var)
        (set! n (add1 n))
        (string-append "$" func-var (number->string n)))))

  (define (extract-func-block! env name wd)
    (hash-set! blocks
               name
               (B-Func name (captured-vars env wd) (extract-block env wd))))
  (define (extract-op-block! env name params wd)
    (define op-env (add-params params env))
    (hash-set! blocks
               name
               (B-OpFunc name params (captured-vars op-env wd) (extract-block op-env wd))))
  (define (extract-ret-block! env name params wd)
    (define ret-env (add-params params env))
    (hash-set! blocks
               name
               (B-RetFunc name params (captured-vars ret-env wd) (extract-block ret-env wd))))

  (define (extract-block-expr env expr)
    (for/list ([w expr]) (extract-block env w)))

  (define (extract-return env params ret-expr)
    (define ret-gen-name (fresh-func-var! "afterward"))
    (extract-ret-block! env ret-gen-name params (C-Statement ret-expr))
    ret-gen-name)

  (define (extract-op env handle-params wd)
    (define f
      (function
       [(C-Operation var params expr)
        (define op-gen-name (fresh-func-var! (substring var 0 (sub1 (string-length var)))))
        (extract-op-block! env op-gen-name (append handle-params params) (C-Statement expr))
        (cons var op-gen-name)]))
    (f wd))
  
  (define (extract-block env wd)
    (define f
      (function
       [(C-Statement expr)
        (B-Statement (extract-block-expr env expr))]
       
       [(C-Assign values scope-wd)
        (B-Assign values (extract-block (add-values values env) scope-wd))]
       
       [(C-Let f-var let-exp let-in)
        (extract-func-block! env f-var let-exp)
        (B-Let f-var (extract-block (add-let f-var env) let-in))]
       
       [(C-LetRec var-exprs let-in)
        (define mutual-names (map first var-exprs))
        (define captured (for/list ([p var-exprs]) (captured-vars-set-expr env (cdr p))))
        (define free (if (andmap null? captured) (Free) (Closure)))
        (define rec-env (add-let-rec mutual-names free env))
        (for ([p var-exprs])
          (extract-func-block! rec-env (car p) (cdr p)))
        (B-LetRec mutual-names (extract-block rec-env let-in))]

       [(C-Handle params handled ops return)
        (B-Handle params
                  (extract-block env handled)
                  (for/list ([o ops]) (extract-op env params o))
                  (extract-return env params return))]
       
       [(C-If then else)
        (B-If (extract-block env then) (extract-block env else))]
       
       [(C-While test body)
        (B-While (extract-block env test) (extract-block env body))]
       
       [(C-FuncVal expr)
        (define f-name (fresh-func-var! "anon"))
        (extract-func-block! env f-name expr)
        (B-FuncVal f-name)]

       [(C-Case clauses otherwise)
        (error "Block language does not yet implement case destructuring")]

       [(C-Var var)
        (B-Var var)]

       [(C-Number n)
        (B-Number n)]

       [a (error "Encountered unsupported block language ast block")]))
    (f wd))

  ; The encompassing block is always given the name 'main' (a reserved word in the grammar)
  (extract-func-block! (hash) "$main" word)
  blocks)