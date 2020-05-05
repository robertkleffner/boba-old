#lang racket/base

(require racket/match)
(require "./lexer.rkt")
(require "./grammar.rkt")

;; BobaUnits = Dictionary<String|Remote, BobaUnit>

;; String -> (BobaUnit, BobaUnits)
(define (pass0-loading input-file-name)
  (define main-unit (load-boba-unit input-file-name))
  (match main-unit
    [`(unit ,imports ,decls (main ,bod))
     (cons main-unit (load-loop (make-immutable-hash) (get-import-paths main-unit)))]
    [mod (error "File specified as entry point must contain a main: " input-file-name)]))

;; BobaUnits * [String|Remote] -> BobaUnits
(define (load-loop program remaining-files)
  (if (null? remaining-files)
      program
      (let-values ([(new-program new-remaining) (load-from-remaining program remaining-files)])
        (load-loop new-program new-remaining))))

;; BobaUnits * [String|Remote] -> BobaUnits, [String|Remote]
(define (load-from-remaining program remaining-files)
  (match remaining-files
    [(list-rest next rem)
     (if (hash-has-key? program next)
         (cons program rem)
         (let ([boba-unit (load-nonmain-boba-unit next)])
           (values (hash-set program next boba-unit) (append rem (get-import-paths boba-unit)))))]
    [(list) (values program (list))]))

;; String|Remote -> BobaUnit
;; Load and parse the boba file at the given local/remote path. Throw an error if it includes a 'main' definition.
(define (load-nonmain-boba-unit unit-path)
  (match (load-boba-unit unit-path)
    [`(unit ,a ... (main ,bod))
     (error "Cannot load a module with another 'main' declaration: " unit-path)]
    [mod mod]))

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
  [(`(unit (imports ,is ...) ,decls ,body))
   (for/list ([i is]
              #:when (equal? 'import (car i)))
     (match i
       [`(import ,names ... ,path ,alias) path]))])

(provide pass0-loading)
(provide load-boba-unit)