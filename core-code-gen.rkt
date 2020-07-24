#lang racket/base

(require racket/match)
(require racket/list)
(require "./lexer.rkt")
(require "./core-grammar.rkt")

;; String -> Program
;; Load and parse the boba file at the given local/remote path.
(provide load-boba-unit)
(define (load-boba-unit unit-path)
  (syntax->datum (parse (tokenize (open-input-file unit-path)))))