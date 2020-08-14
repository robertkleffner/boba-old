#lang algebraic/racket/base

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



