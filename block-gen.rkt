#lang algebraic/racket/base

(require racket/set)
(require racket/list)
(require racket/pretty)
(module+ test
  (require rackunit))

; The primary purpose of this file is to translate a set of Boba-blocks program into
; a flat list of Bubble 'byte code' instructions. Boba-blocks are themselves flat,
; except for some nested structure around variable values, conditionals, and handlers,
; so the translation is mostly straight-forward. The major complexity in this translation
; is still probably closure conversion.

; Notable assumptions made in the algorithms and data-structures of this file:
; 1) Each block definition should have a globally unique name, and the same
; goes for constructor names. This allows them to be put into a hash table.
; This is less important for value variable names, which are still tracked
; through an scoped environment in this step before being erased entirely
; into frame offsets.
; 2) Each block definition explicitly lists it's 'capturable' free variables.
; These free variables may be value variables, or calls to closures or continuations
; that are within scope. Note that top-level functions are almost always guaranteed
; to NOT be closures, so they should not be listed in free variables of other
; block definitions.



;; type Block-Program = (Ctors, Blocks)

;; type Blocks = { FuncVar -> Def }

(provide B-Func B-OpFunc B-RetFunc)
(data BlockDef (B-Func ; FuncVar, [FuncVar|ValVar], Word
                B-OpFunc ; FuncVar, [ValVar], [FuncVar|ValVar], Word
                B-RetFunc ; FuncVar, [ValVar], [FuncVar|ValVar], Word
                ))

;; type Expr = [Word]

(provide B-Statement B-Assign B-Let B-LetRec B-Handle B-If B-While B-FuncVal
         B-Var B-Destruct B-Number)
(data Word (B-Statement ; Expr
            B-Assign ; [ValVar], Word
            B-Let ; FuncVar, Word
            B-LetRec ; [FuncVar], Word
            B-Handle ; [ValVar], Expr, [(OpVar, FuncVar)], FuncVar
            B-If ; Expr Expr
            B-While ; Expr Expr
            B-FuncVal ; FuncVar
            B-Var ; ValVar | OpVar | FuncVar | ConVar | PredVar
            B-Destruct
            B-Number ; Number
            ))



(define PRIMS
  (hash "$newref" '((newref))
        "$getref" '((getref))
        "$putref" '((putref))

        "$do" '((call-closure))

        "$add-i32" '((add-i32))
        "$sub-i32" '((sub-i32))
        "$mul-i32" '((mul-i32))
        "$div-i32" '((div-i32))

        "$bool-and" '((bool-and))
        "$bool-or" '((bool-or))
        "$bool-not" '((bool-not))
        "$bool-xor" '((bool-xor))

        "$list-nil" '((list-nil))
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
     [(B-Func name free wd)
      free]
     [(B-OpFunc name args free wd)
      free]
     [(B-RetFunc name args free wd)
      free]))
  (f (hash-ref defs var)))

;; op-args : FuncVar, { FuncVar -> Def } -> [ValVar]
(define (op-args var defs)
  (define f
    (function
     [(B-OpFunc fvar params wd)
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
     [(B-Func name free wd)
      `(,(append (list*
                  'label
                  `',(string->symbol name)
                  (gen-word wd ctors defs (list free)))
                 '((return))))]
     [(B-OpFunc name params free wd)
      (define initial-frame (cons "$resume" (append params free)))
      `(,(append (list*
                  'label
                  `',(string->symbol name)
                  (gen-word wd ctors defs (list initial-frame)))
                 '((return))))]
     [(B-RetFunc name params free wd)
      (define initial-frame (append params free))
      `(, (append (list*
                   'label
                   `',(string->symbol name)
                   (gen-word wd ctors defs (list initial-frame)))
                  '((return))))]))
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
     [(B-Statement expr)
      (gen-expr expr ctors defs env)]
     [(B-Assign vars wd)
      (append
       `((store ,(length vars)))
       (gen-word wd ctors defs (cons vars env))
       `((forget)))]

     ;; if there are no free variables for this word, it can be called directly, and doesn't need to be stored as a closure
     [(B-Let var wd)
      (cond
        [(null? (def-free-vars var defs))
         (gen-word wd ctors defs env)]
        [else
         (append
          `((closure ',(string->symbol var)
                     ,(gen-capture-args (def-free-vars var defs) env)))
          `((store 1))
          (gen-word wd ctors defs (cons '(var) env))
          `((forget)))])]

     ;; if none of the mutual words have free variables, they can all be called directly, and don't need to be stored as closures
     [(B-LetRec vars wd)
      (cond
        [(for/and ([v vars]) (null? (def-free-vars v defs)))
         (gen-word wd ctors defs env)]
        [else
         (define closures
           (for/list ([v vars])
             `(closure ',(string->symbol v)
                       ,(gen-capture-args (def-free-vars v defs) env))))
         (append
          closures
          `((mutual ,(length vars)))
          `((store ,(length vars)))
          (gen-word wd ctors defs (cons vars env))
          `((forget)))])]
     [(B-Handle params body ops return)
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
     [(B-If then else)
      (define gen-then (gen-expr then ctors defs env))
      (append
       `((offset-if ,(add1 (length gen-then))))
       gen-then
       (gen-expr else ctors defs env))]
     [(B-While test body)
      (define gen-body (gen-expr body ctors defs env))
      (define gen-test (gen-expr test ctors defs env))
      (append
       gen-test
       `((offset-if ,(add1 (add1 (length gen-body)))))
       gen-body
       `((offset ,(negate (add1 (+ (length gen-test) (length gen-body)))))))]
     [(B-FuncVal name)
      `((closure ',(string->symbol name)
                 ,(gen-capture-args (def-free-vars name defs) env)))]
     [(B-Var name)
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
     [(B-Destruct)
      `((destruct))]
     [(B-Number n)
      `((push ,n))]
     [a (error "Cannot generate word")]))
  (f word))

;; gen-to-file : String, Ctors, Defs -> ()
(define (gen-to-file path ctors defs)
  (with-output-to-file path #:exists 'truncate/replace
    (λ () (pretty-write (gen-program ctors defs)))))
