#lang racket/base

(require racket/match)
(require "./lexer.rkt")
(require "./grammar.rkt")

;; BobaProgram = Dictionary<String|Remote, BobaUnit>

;; String -> BobaProgram
(define (pass0-loading input-file-name)
  (load-loop (make-immutable-hash) (list input-file-name)))

;; BobaProgram * [String|Remote] -> BobaProgram
(define (load-loop program remaining-files)
  (if (null? remaining-files)
      program
      (let-values ([(new-program new-remaining) (load-from-remaining program remaining-files)])
        (load-loop new-program new-remaining))))

;; BobaProgram * [String|Remote] -> BobaProgram, [String|Remote]
(define (load-from-remaining program remaining-files)
  (match remaining-files
    [(list-rest next rem)
     (if (hash-has-key? program next)
         (cons program rem)
         (let ([boba-unit (load-boba-unit next)])
           (values (hash-set program next boba-unit) (append rem (get-import-paths boba-unit)))))]
    [(list) (values program (list))]))

;; String|Remote -> BobaUnit
;; Load and parse the boba file at the given local/remote path.
(define (load-boba-unit unit-path)
  (syntax->datum (parse (tokenize (make-boba-port unit-path)))))

;; String|Remote -> Port
;; Create a port to a boba file from either a local (string) path, or a remote (URN) path.
(define/match (make-boba-port unit-path)
  [((? string? local-name)) (open-input-file local-name)]
  [(`(remote ,org ,proj ,mod ,maj, min, patch))
   (open-input-file (string-append org "-" proj "-" mod "-" maj "-" min "-" patch))])

;; BobaUnit -> [String|Remote]
;; Extract all the import paths from the boba AST.
(define/match (get-import-paths boba-unit)
  [(`(unit ,decls ...))
   (for/list ([d decls]
              #:when (equal? 'import (car d)))
     (match d
       [`(import ,path ,alias) path]
       [`(import ,names ,path ,alias) path]))])

(provide pass0-loading)