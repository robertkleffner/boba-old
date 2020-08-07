#lang racket/base

(require racket/set)
(require racket/match)
(require racket/list)
(require "./lexer.rkt")
(require "./core-grammar.rkt")

;; String -> Program
;; Load and parse the boba file at the given local/remote path.
(provide load-boba-unit)
(define (load-boba-unit unit-path)
  (syntax->datum (parse (tokenize (open-input-file unit-path)))))

(define/match (free-vars expr)
  [(`(term-statement-block ,expr))
   (free-vars expr)]
  [(`(simple-expr ,words ...))
   (apply set-union (map free-vars words))]
  [(`(let-word (term-variable ,let-name) ,let-e ,let-in))
   (set-union (free-vars let-e)
              (set-remove (free-vars let-in) let-name))]
  [(`(let-rec-word ,defs ,let-in))
   ;(define names (list->set (map get-rec-name defs)))
   ;(set-union (free-in-defs defs)
   ;           (set-subtract (free-vars let-in) names))]
   (set)]
  [(`(handle-word (handle-params ,params) ,handled ,handlers ... ,return))
   (define ps (apply set-union (map free-vars params)))
   (set-union (free-vars handled)
              (set-subtract (apply set-union (map free-vars handlers)) ps)
              (set-subtract (free-vars return) ps))]
  [(`(if-word ,condition ,then ,else))
   (set-union (free-vars condition) (free-vars then) (free-vars else))]
  [(`(while-word ,condition ,body))
   (set-union (free-vars condition) (free-vars body))]
  [(`(function-literal ,expr))
   (free-vars expr)]
  [(`(identifier ,qualifiers ... (term-variable ,name)))
   (if (null? qualifiers)
       (set name)
       (set))]
  [(_)
   (set)])

(define prims
  (hash "add-i32" '((add-i32))
        "sub-i32" '((sub-i32))))

(define (compile-expr env expr)
  (append*
   (for/list ([w expr])
     (compile-word env w))))

;; compile-word : List (List (String . Is-Seq-Var)), Word -> List Instr

(define (compile-word env word)
  (match word
    [(? number? n)
     '((push ,n))]
    [`(term-variable ,name)
     (if (hash-has-key? prims name)
         (hash-ref prims name)
         (let-values ([(frame index is-seq) (find-env env name)])
           (if is-seq
               '((find ,frame ,index) (call-closure))
               '((find ,frame ,index)))))]
    [`(let-word (term-variable ,name) (simple-expr ,words ...) ,let-word)
     (define captured (set->list (free-vars '(simple-expr ,words))))
     (define label (compile-block name words captured))
     (append
      '((closure ,label ,(compile-capture-args captured)))
      '((store 1))
      (compile-word (cons (list (cons name #t)) env) let-word)
      '((forget)))]
    [`(assign-word (term-variable ,names) ... ,let-word)
     (append
      '((store ,(length names)))
      (compile-word (cons (map (lambda (n) (cons n #f)) names) env) let-word)
      '((forget)))]
    [`(function-literal (simple-expr ,words ...))
     (define captured (set->list (free-vars '(simple-expr ,words))))
     (define label (compile-block null words captured))
     '((closure ,label (compile-capture-args captured)))]
    ["do"
     '((call-closure))]))

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
  

(define (compile-block name expr captured)
  null)