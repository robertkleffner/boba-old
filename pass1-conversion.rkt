#lang racket/base

(require racket/match)
(require "./lexer.rkt")
(require "./grammar.rkt")
(require "./common.rkt")
(require "./pass1-ast.rkt")



(define (pass1-convert program)
  (cons
   (p1-unit (car program))
   (for/hash ([(k v) (in-hash (cdr program))])
     (values (p1-convert-import-path k) (p1-unit v)))))
(provide pass1-convert)

(define/match (p1-unit boba-unit)
  [(`(unit (imports ,is ...) (declarations ,ds ...) (export ,ns ...)))
   (P1-Boba-Unit
    (map p1-import is)
    (map p1-decl ds)
    ns)]

  [(`(unit (imports ,is ...) (declarations ,ds ...) (main ,body)))
   (P1-Boba-Main
    (map p1-import is)
    (map p1-decl ds)
    (p1-expr body))]

  [(x) (error "Invalid unit form: " x)])

(define/match (p1-import import)
  [(`(import ,ns ... ,path ,alias))
   (P1-Import ns (p1-convert-import-path path) alias)]

  [(x) (error "Invalid import form: " x)])

(define/match (p1-convert-import-path path)
  [(`(remote ,org ,proj ,name ,major ,minor ,patch))
   (P1-Remote-Path org proj name major minor patch)]
  [((? string? p)) p]
  [(x) (error "Invalid import path: " x)])

(define/match (p1-decl decl)
  [(`(function (term-variable ,name) ,body))
   (P1-Function name null (p1-expr body))]
  [(`(function (term-variable ,name) (fixed-size-params (term-variable ,params) ...) ,body))
   (P1-Function name params (p1-expr body))]
  [(x) (error "Invalid decl: " x)])

(define/match (p1-expr expr)
  [(`(simple-expr ,words ...))
   (map p1-word words)])
                      
(define/match (p1-word word)
  [(`(identifier (term-variable ,name)))
   (P1-Term-Variable name (None) (None))]
  [(`(identifier (term-variable ,alias) (term-variable ,name)))
   (P1-Term-Variable name alias (None))])