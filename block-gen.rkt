#lang algebraic/racket/base

(require racket/set)
(require racket/list)



(data Def (Func ; FuncVar, Word
           Closure ; FuncVar, [FuncVar], Word
           OpFunc ; FuncVar, [ValVar], Word
           OpClosure ; FuncVar, [ValVar], [ValVar], Word
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



(define prims
  (hash "add-i32" '((add-i32))
        "sub-i32" '((sub-i32))))



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

;; expr-free-vars : Expr -> Set Var
(define (expr-free-vars expr) (apply set-union (map free-vars expr)))

;; free-vars : Word -> Set Var
(define free-vars
  (function
   [(Statement expr)
    (expr-free-vars expr)]
   [(Assign vars wd)
    (set-subtract (free-vars wd) (list->set vars))]
   [(Let var wd)
    (set-remove (free-vars wd) var)]
   [(LetRec vars wd)
    (set-subtract (free-vars wd) (list->set vars))]
   [(Handle params handled ops ret)
    (free-vars handled)]
   [(If then else)
    (set-union (free-vars then) (free-vars else))]
   [(While check body)
    (set-union (free-vars check) (free-vars body))]
   [(FuncVal name)
    (set name)]
   [(Var name)
    (set name)]
   [a (set)]))

(define (in-env env name)
  (ormap (λ (f) (member name f)) env))

;; find-env : Env, String -> Int, Int
(define (find-env env name)
  (define (find-env-rec env frame index)
    (cond
      ;; end of the environment and haven't found it?
      [(null? env) (error "Couldn't find name in environment")]
      ;; searched through current frame?
      [(null? (first env))
       (find-env-rec (rest env) (add1 frame) 0)]
      ;; found it in current frame?
      [(string=? name (car (first (first env))))
       (values frame index (cdr (first (first env))))]
      ;; next variable in current frame is not the target?
      [else
       (find-env-rec (cons (rest (first env)) (rest env)) frame (add1 index))]))
  (find-env-rec env 0 0))

(define (gen-capture-args free env)
  (apply
   values
   (for/list ([f free]
              #:when (in-env env f))
     (define-values (frame index) (find-env env f))
     `(cons ,frame ,index))))

(define (def-free-vars var defs)
  (define f
    (function
     [(Top fvar wd)
      (free-vars wd)]
     [(Local fvar included wd)
      (set-subtract (free-vars wd) (list->set fvar))]
     [(Op fvar params wd)
      (set-subtract (free-vars wd) (list->set fvar))]))
  (f (hash-ref defs var)))

(define (op-args var defs)
  (define f
    (function
     [(Op fvar params wd)
      params]))
  (length (f (hash-ref defs var))))

(define (gen-def def ctors defs)
  (function
   [(Top fvar wd)
    `(label ,(string->symbol fvar)
            ,(apply values (gen-word wd ctors defs null)))]
   [(

;; gen-expr : Expr, Ctors, Defs, Env -> [BubbleInstr]
(define (gen-expr expr ctors defs env)
  (append*
   (for/list ([w expr])
     (gen-word w ctors defs env))))

;; gen-word : Word, Ctors, Defs, Env -> [BubbleInstr]
(define (gen-word word ctors defs env)
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
     `((closure ,(string->symbol var)
                ,(gen-capture-args (def-free-vars var defs) env)))
     `((store 1))
     (gen-word wd ctors defs (cons '(var) env))
     `((forget)))]
   [(LetRec vars wd)
    (define closures
      (for/list ([v vars])
        `(closure ,(string->symbol v)
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
        `(op-closure ,(string->symbol o)
                     ,(op-args o defs)
                     ,(gen-capture-args (def-free-vars o defs) env))))
    (define gen-body (gen-expr body ctors defs env))
    (append
     `((closure ,(string->symbol return)
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
    `((closure ,(string->symbol name)
               ,(gen-capture-args (def-free-vars name defs) env)))]
   [(Var name)
    (cond
      [(string=? name "$resume")
       (define-values (frame index) (find-env env name))
       `((find ,frame ,index)
         (call-continuation))]
      [(op-var? name)
       `((operation ,(string->symbol name)))]
      [(func-var? name)
       (cond
         [(hash-has-key? prims name)
          (hash-ref prims name)]
         [(in-env env name)
          (define-values (frame index) (find-env env name))
          `((find ,frame ,index)
            (call-closure))]
         [else
          `(call ,(string->symbol name))])]
      [(con-var? name)
       `((construct ,(string->symbol name)
                    ,(hash-ref ctors name)))]
      [(pred-var? name)
       `((is-struct ,(string->symbol name)))]
      [else
       (define-values (frame index) (find-env env name))
       `((find ,frame ,index))])]
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
         