#lang algebraic/racket/base

(require racket/set)
(require racket/list)
(require racket/pretty)
(module+ test
  (require rackunit))



;; type Program = { FuncVar -> Def }

(data Def (Func ; FuncVar, [FuncVar|ValVar], Word
           OpFunc ; FuncVar, [ValVar], [FuncVar|ValVar], Word
           RetFunc ; FuncVar, [ValVar], [FuncVar|ValVar], Word
           ))

;; type Expr = [Word]

(data Word (Statement ; Expr
            Assign ; [ValVar], Word
            Let ; FuncVar, Word
            LetRec ; [FuncVar], Word
            Handle ; [ValVar], Expr, [(OpVar, FuncVar)], FuncVar
            If ; Expr Expr
            While ; Expr Expr
            FuncVal ; FuncVar
            ListVal
            NewRef
            GetRef
            PutRef
            Var ; ValVar | OpVar | FuncVar | ConVar
            Do
            Destruct
            Number ; Number
            ))



(define PRIMS
  (hash "$add-i32" '((add-i32))
        "$sub-i32" '((sub-i32))
        "$mul-i32" '((mul-i32))
        "$div-i32" '((div-i32))

        "$bool-and" '((bool-and))
        "$bool-or" '((bool-or))
        "$bool-not" '((bool-not))
        "$bool-xor" '((bool-xor))

        "$list-cons" '((list-cons))
        "$list-snoc" '((list-snoc))
        "$list-head" '((list-head))
        "$list-last" '((list-last))
        "$list-tail" '((list-tail))
        "$list-init" '((list-init))
        "$list-append" '((list-append))
        "$list-empty" '((list-empty))))



(define (last-char str)
  (string-ref str (sub1 (string-length str))))

(define (first-char str)
  (string-ref str 0))

(define (op-var? x)
  (and (string? x) (char=? #\! (last-char x))))

(define (func-var? x)
  (and (string? x) (char=? #\$ (first-char x))))

(define (con-var? x)
  (and (string? x) (char-upper-case? (first-char x))))

(define (pred-var? x)
  (and (string? x) (char=? #\? (last-char x))))

(module+ test
  (check-true (op-var? "raise!"))
  (check-false (op-var? "raise"))
  (check-true (func-var? "$foo"))
  (check-false (func-var? "foo")))



;; in-env : Env, String -> Bool
(define (in-env env name)
  (ormap (λ (f) (member name f)) env))

(module+ test
  (check-false (in-env null "hello"))
  (check-not-false (in-env (list (list "hello")) "hello"))
  (check-not-false (in-env (list null (list "hello")) "hello"))
  (check-not-false (in-env (list (list "hello") (list "hello" "gbye")) "gbye")))



;; find-env : Env, String -> (Int, Int)
(define (find-env env name)
  (define (find-env-rec env frame index)
    (cond
      ;; end of the environment and haven't found it?
      [(null? env) (error "Couldn't find name in environment")]
      ;; searched through current frame?
      [(null? (first env))
       (find-env-rec (rest env) (add1 frame) 0)]
      ;; found it in current frame?
      [(string=? name (first (first env)))
       (cons frame index)]
      ;; next variable in current frame is not the target?
      [else
       (find-env-rec (cons (rest (first env)) (rest env)) frame (add1 index))]))
  (find-env-rec env 0 0))

(module+ test
  (check-equal? (find-env (list (list "hello")) "hello")
                (cons 0 0))
  (check-equal? (find-env (list (list "hello" "gbye")) "gbye")
                (cons 0 1))
  (check-equal? (find-env (list (list "hello") (list "gbye")) "gbye")
                (cons 1 0)))



(define (gen-capture-args free env)
  (apply
   values
   (for/list ([f free]
              #:when (in-env env f))
     (find-env env f))))

;; def-free-vars : FuncVar, { FuncVar -> Def } -> [FuncVar|ValVar]
(define (def-free-vars var defs)
  (define f
    (function
     [(Func name free wd)
      free]
     [(OpFunc name args free wd)
      free]
     [(RetFunc name args free wd)
      free]))
  (f (hash-ref defs var)))

;; op-args : FuncVar, { FuncVar -> Def } -> [ValVar]
(define (op-args var defs)
  (define f
    (function
     [(Op fvar params wd)
      params]))
  (length (f (hash-ref defs var))))
      


;; gen-program : Ctors, Defs -> [BubbleInstr]
(define (gen-program ctors defs)
  (append
   `((call ',(string->symbol "$main"))
     (jump ',(string->symbol "end")))
   (append*
    (for/list ([d (in-hash-values defs)])
      (gen-def d ctors defs)))
   `((label ',(string->symbol "end")
            (nop)))))

;; gen-def : Def, Ctors, Defs -> [BubbleInstr]
(define (gen-def def ctors defs)
  (define f
    (function
     [(Func name free wd)
      `(,(list* 'label `',(string->symbol name) (gen-word wd ctors defs (list free))))]
     [(OpFunc name params free wd)
      (define initial-frame (cons "$resume" (append params free)))
      `((label ',(string->symbol name)
               ,(apply values (gen-word wd ctors defs (list initial-frame)))))]
     [(RetFunc name params free wd)
      (define initial-frame (append params free))
      `((label ',(string->symbol name)
               ,(apply values (gen-word wd ctors defs (list initial-frame)))))]))
  (f def))
   
;; gen-expr : Expr, Ctors, Defs, Env -> [BubbleInstr]
(define (gen-expr expr ctors defs env)
  (append*
   (for/list ([w expr])
     (gen-word w ctors defs env))))

;; gen-word : Word, Ctors, Defs, Env -> [BubbleInstr]
(define (gen-word word ctors defs env)
  (define f
    (function
     [(Statement expr)
      (gen-expr expr ctors defs env)]
     [(Assign vars wd)
      (append
       `((store ,(length vars)))
       (gen-word wd ctors defs (cons vars env))
       `((forget)))]
     [(Let var wd)
      (append
       `((closure ',(string->symbol var)
                  ,(gen-capture-args (def-free-vars var defs) env)))
       `((store 1))
       (gen-word wd ctors defs (cons '(var) env))
       `((forget)))]
     [(LetRec vars wd)
      (define closures
        (for/list ([v vars])
          `(closure ',(string->symbol v)
                    ,(gen-capture-args (def-free-vars v defs) env))))
      (append
       closures
       `((mutual ,(length vars)))
       `((store ,(length vars)))
       (gen-word wd ctors defs (cons vars env))
       `((forget)))]
     [(Handle params body ops return)
      (define ops
        (for/list ([o (map cdr ops)])
          `(op-closure ',(string->symbol o)
                       ,(op-args o defs)
                       ,(gen-capture-args (def-free-vars o defs) env))))
      (define gen-body (gen-expr body ctors defs env))
      (append
       `((closure ',(string->symbol return)
                  ,(gen-capture-args (def-free-vars return defs) env)))
       ops
       `((handle ,(add1 (length gen-body))
                 ,(length params)
                 ,(map (λ (o) (string->symbol (car o))) ops)))
       body
       `((complete)))]
     [(If then else)
      (define gen-then (gen-expr then ctors defs env))
      (append
       `((offset-if ,(add1 (length gen-then))))
       gen-then
       (gen-expr else ctors defs env))]
     [(While test body)
      (define gen-body (gen-expr body ctors defs env))
      (define gen-test (gen-expr test ctors defs env))
      (append
       gen-test
       `((offset-if ,(add1 (add1 (length gen-body)))))
       gen-body
       `((offset ,(negate (add1 (+ (length gen-test) (length gen-body)))))))]
     [(FuncVal name)
      `((closure ',(string->symbol name)
                 ,(gen-capture-args (def-free-vars name defs) env)))]
     [(Var name)
      (cond
        [(string=? name "$resume")
         (define loc (find-env env name))
         `((find ,(car loc) ,(cdr loc))
           (call-continuation))]
        [(op-var? name)
         `((operation ',(string->symbol name)))]
        [(func-var? name)
         (cond
           [(hash-has-key? PRIMS name)
            (hash-ref PRIMS name)]
           [(in-env env name)
            (define loc (find-env env name))
            `((find ,(car loc) ,(cdr loc))
              (call-closure))]
           [else
            `(call ',(string->symbol name))])]
        [(con-var? name)
         `((construct ',(string->symbol name)
                      ,(hash-ref ctors name)))]
        [(pred-var? name)
         `((is-struct ',(string->symbol name)))]
        [else
         (define loc (find-env env name))
         `((find ,(car loc) ,(cdr loc)))])]
     [(ListVal)
      `((list-nil))]
     [(NewRef)
      `((newref))]
     [(GetRef)
      `((getref))]
     [(PutRef)
      `((putref))]
     [(Do)
      `((call-closure))]
     [(Destruct)
      `((destruct))]
     [(Number n)
      `((push ,n))]
     [a (error "Cannot generate word")]))
  (f word))

;; gen-to-file : String, Ctors, Defs -> ()
(define (gen-to-file path ctors defs)
  (with-output-to-file path #:exists 'truncate/replace
    (λ () (pretty-write (gen-program ctors defs)))))
