#lang typed/racket

(require "./common.rkt")
(require "./pass1-ast.rkt")



(define-type P2-Boba-Program
  (Pair P2-Boba-Main (Immutable-HashTable (U String P2-Remote-Path) P2-Boba-Unit)))
(provide P2-Boba-Program)

(define-type P2-Boba-Units
  (Immutable-HashTable (U String P2-Remote-Path) P2-Boba-Unit))
(provide P2-Boba-Units)

(define-type P2-Boba-Unit (Listof P2-Declaration))
(provide P2-Boba-Unit)

(define-struct P2-Boba-Main
  ([declarations : P2-Boba-Unit]
   [main : (Listof P2-Word)])
  #:transparent)
(provide (struct-out P2-Boba-Main))



(define-type P2-Import-Path (U String P2-Remote-Path))
(provide P2-Import-Path)

(: p2-boba-path->string (-> P2-Import-Path String))
(define (p2-boba-path->string path)
  (match path
    [(P2-Remote-Path org proj name major minor patch)
     (string-append org "." proj "." name ":" (number->string major) "." (number->string minor) "." (number->string patch))]
    [(? string? x) x]))
(provide p2-boba-path->string)

(define-struct P2-Remote-Path
  ([org : String]
   [proj : String]
   [name : String]
   [major : Integer]
   [minor : Integer]
   [patch : Integer])
  #:transparent)
(provide (struct-out P2-Remote-Path))



(define-type P2-Declaration
  (U P2-Data
     P2-Data-Rec
     P2-Pattern-Synonym
     P2-Adhoc
     P2-Overload
     P2-Derive
     P2-Tag
     P2-Type-Synonym
     P2-Effect-Synonym
     P2-Predicate-Synonym
     P2-Check
     P2-Function
     P2-Recursive
     P2-Test
     P2-Law))
(provide P2-Declaration)



(define-struct P2-Data
  ([name : String]
   [tag : (Opt String)]
   [params : (Listof String)]
   [constructors : (Listof P2-Constructor)])
  #:transparent)
(provide (struct-out P2-Data))

(define-struct P2-Data-Rec
  ([mutuals : (Listof P2-Data)])
  #:transparent)
(provide (struct-out P2-Data-Rec))

(define-struct P2-Constructor
  ([name : String]
   [elems : (Listof P2-Type)])
  #:transparent)
(provide (struct-out P2-Constructor))



(define-struct P2-Pattern-Synonym
  ([name : String]
   [params : (Listof String)]
   [pattern : P2-Pattern])
  #:transparent)
(provide (struct-out P2-Pattern-Synonym))



(define-struct P2-Adhoc
  ([name : String]
   [predicate-name : String]
   [qualified-var : String]
   [type : P2-Type])
  #:transparent)
(provide (struct-out P2-Adhoc))

(define-struct P2-Overload
  ([name : String]
   [instance : P2-Predicate]
   [context : (Listof P2-Predicate)]
   [body : (Listof P2-Word)])
  #:transparent)
(provide (struct-out P2-Overload))

(define-struct P2-Derive
  ([name : String]
   [instance : P2-Type]
   [context : (Listof P2-Type)])
  #:transparent)
(provide (struct-out P2-Derive))



(define-struct P2-Tag
  ([name : String]
   [tag : String])
  #:transparent)
(provide (struct-out P2-Tag))



(define-struct P2-Type-Synonym
  ([name : String]
   [params : (Listof String)]
   [type : P2-Type])
  #:transparent)
(provide (struct-out P2-Type-Synonym))

(define-struct P2-Effect-Synonym
  ([name : String]
   [params : (Listof String)]
   [effects : (Listof P2-Effect)])
  #:transparent)
(provide (struct-out P2-Effect-Synonym))

(define-struct P2-Predicate-Synonym
  ([name : String]
   [param : String]
   [predicates : (Listof P2-Predicate)])
  #:transparent)
(provide (struct-out P2-Predicate-Synonym))



(define-struct P2-Check
  ([name : String]
   [type : P2-Type])
  #:transparent)
(provide (struct-out P2-Check))

(define-struct P2-Function
  ([name : String]
   [fixed : (Listof String)]
   [body : (Listof P2-Word)])
  #:transparent)
(provide (struct-out P2-Function))

(define-struct P2-Recursive
  ([mutual : (Listof P2-Function)])
  #:transparent)
(provide (struct-out P2-Recursive))



(define-struct P2-Test
  ([name : String]
   [left : (Listof P2-Word)]
   [right : (Listof P2-Word)]
   [type : String]
   [compare : (Opt P2-Block)])
  #:transparent)
(provide (struct-out P2-Test))

(define-struct P2-Law
  ([name : String]
   [exhaustive : Boolean]
   [left : (Listof P2-Word)]
   [right : (Listof P2-Word)]
   [type : String]
   [compare : (Opt P2-Block)])
  #:transparent)
(provide (struct-out P2-Law))



(define-struct P2-Predicate
  ([name : String]
   [alias : P2-Import-Path]
   [type : P2-Type]
   [dotted : Boolean])
  #:transparent)
(provide (struct-out P2-Predicate))

(define-type P2-Type
  (U P2-Type-App
     P2-Tag-Type
     P2-Fixed-Size-Type
     P2-Type-Seq
     P2-Field-Type
     P2-Effect-Seq
     P2-Type-Cons
     P2-Type-Var))
(provide P2-Type)

(define-struct P2-Type-App
  ([left : P2-Type]
   [right : P2-Type])
  #:transparent)
(provide (struct-out P2-Type-App))

(define-struct P2-Tag-Type
  ([variables : (Listof P2-Abelian-Var)]
   [constructor : (Listof P2-Abelian-Cons)])
  #:transparent)
(provide (struct-out P2-Tag-Type))

(define-struct P2-Fixed-Size-Type
  ([variables : (Listof P2-Abelian-Var)]
   [constant : Integer])
  #:transparent)
(provide (struct-out P2-Fixed-Size-Type))

(define-struct P2-Type-Seq
  ([elems : (Listof P2-Type)]
   [dotted : (Opt P2-Type)])
  #:transparent)
(provide (struct-out P2-Type-Seq))

(define-struct P2-Field-Seq
  ([fields : (Listof P2-Field-Type)]
   [dotted : String])
  #:transparent)
(provide (struct-out P2-Field-Seq))

(define-struct P2-Field-Type
  ([field : String]
   [type : P2-Type])
  #:transparent)
(provide (struct-out P2-Field-Type))

(define-struct P2-Effect-Seq
  ([effects : (Listof P2-Effect)]
   [dotted : String])
  #:transparent)
(provide (struct-out P2-Effect-Seq))

(define-struct P2-Effect
  ([op : String]
   [params : (Listof P2-Type)])
  #:transparent)
(provide (struct-out P2-Effect))

(define-struct P2-Abelian-Var
  ([name : String]
   [exponent : Integer])
  #:transparent)
(provide (struct-out P2-Abelian-Var))

(define-struct P2-Abelian-Cons
  ([name : String]
   [alias : P2-Import-Path]
   [exponent : Integer])
  #:transparent)
(provide (struct-out P2-Abelian-Cons))

(define-struct P2-Type-Cons
  ([name : String]
   [alias : P2-Import-Path])
  #:transparent)
(provide (struct-out P2-Type-Cons))

(define-struct P2-Type-Var
  ([name : String])
  #:transparent)
(provide (struct-out P2-Type-Var))



(define-type P2-Word
  (U P2-Block
     P2-Local
     P2-Local-Mutual
     P2-Handle
     P2-Match
     P2-If
     P2-While
     P2-Until
     P2-For-Comp
     P2-Anonymous
     P2-Tuple-Lit
     P2-List-Lit
     P2-Vector-Lit
     P2-Slice-Lit
     P2-Dict-Lit
     P2-Record-Lit
     P2-Extension
     P2-Restriction
     P2-Selection
     P2-Update
     P2-Variant-Lit
     P2-Embed
     P2-Case
     P2-Bag-Lit
     P2-Bag-Get
     P2-Bag-Put
     P2-Bag-Drop
     P2-Union-Lit
     P2-Typeof
     P2-With-State
     P2-Newref
     P2-Getref
     P2-Putref
     P2-Untag
     P2-Term-Constructor
     P2-Term-Variable
     P2-Operator
     P2-Do
     String
     Char
     Integer
     Flonum))
(provide P2-Word)
     


(define-struct P2-Block
  ([statements : (Listof P2-Statement)])
  #:transparent)
(provide (struct-out P2-Block))

(define-type P2-Statement (U P2-Let-Statement (Listof P2-Word)))
(provide P2-Statement)

(define-struct P2-Let-Statement
  ([patterns : (Listof P2-Pattern)]
   [body : (Listof P2-Word)])
  #:transparent)
(provide (struct-out P2-Let-Statement))



(define-struct P2-Local
  ([name : String]
   [body : (Listof P2-Word)]
   [in : P2-Word])
  #:transparent)

(define-struct P2-Local-Mutual
  ([mutuals : (Pair String (Listof P2-Word))]
   [in : P2-Word])
  #:transparent)



(define-struct P2-Handle
  ([params : (Listof String)]
   [body : P2-Block]
   [handlers : (Listof P2-Handler)]
   [return : (Listof P2-Word)])
  #:transparent)

(define-struct P2-Handler
  ([op : String]
   [params : (Listof String)]
   [body : (Listof P2-Word)])
  #:transparent)



(define-struct P2-Match
  ([branches : (U (Listof P2-Rigid-Branch) (Listof P2-Flex-Branch))]
   [otherwise : (Listof P2-Word)])
  #:transparent)

(define-struct P2-Rigid-Branch
  ([patterns : (Listof P2-Pattern)]
   [body : (Listof P2-Word)])
  #:transparent)

(define-struct P2-Flex-Branch
  ([patterns : (Listof P2-Pattern)]
   [dotted : P2-Pattern]
   [body : (Listof P2-Word)])
  #:transparent)



(define-struct P2-If
  ([condition : P2-Block]
   [then : P2-Block]
   [else : P2-Block])
  #:transparent)

(define-struct P2-While
  ([condition : P2-Block]
   [body : P2-Block])
  #:transparent)

(define-struct P2-Until
  ([condition : P2-Block]
   [body : P2-Block])
  #:transparent)



(define-struct P2-For-Comp
  ([kind : String]
   [fors : (Listof P2-For-Clause)]
   [breaks : (Listof P2-Break-Clause)]
   [body : P2-Block])
  #:transparent)

(define-type P2-For-Clause
  (U P2-For-Select
     P2-For-When
     P2-For-Length
     P2-For-Result
     P2-For-Break
     P2-For-Final))

(define-struct P2-For-Select
  ([name : String]
   [body : (Listof P2-Word)])
  #:transparent)

(define-struct P2-For-When
  ([body : P2-Statement]
   [negated : Boolean])
  #:transparent)

(define-struct P2-For-Length
  ([size : P2-Fixed-Term]
   [fill : (Listof P2-Word)])
  #:transparent)

(define-struct P2-For-Result
  ([result : (Listof P2-Word)])
  #:transparent)

(define-type P2-Break-Clause
  (U P2-For-Break
     P2-For-Final))

(define-struct P2-For-Break
  ([break : (Listof P2-Word)])
  #:transparent)

(define-struct P2-For-Final
  ([break : (Listof P2-Word)])
  #:transparent)



(define-struct P2-Anonymous
  ([body : (Listof P2-Word)])
  #:transparent)



(define-struct P2-Tuple-Lit
  ([elems : (Listof P2-Word)]
   [rest : (Opt P2-Word)])
  #:transparent)

(define-struct P2-List-Lit
  ([left : (Listof P2-Word)]
   [right : (Listof P2-Word)]
   [rest : (Opt P2-Word)])
  #:transparent)

(define-struct P2-Vector-Lit
  ([left : (Listof P2-Word)]
   [right : (Listof P2-Word)]
   [rest : (Opt P2-Word)])
  #:transparent)

(define-struct P2-Slice-Lit
  ([subslice : (Opt P2-Slice-Lit)]
   [start : P2-Word]
   [end : P2-Word])
  #:transparent)

(define-struct P2-Dict-Lit
  ([fields : (Listof (Pair P2-Word P2-Word))]
   [rest : (Opt P2-Word)])
  #:transparent)



(define-struct P2-Record-Lit
  ([fields : (Listof (Pair String P2-Word))]
   [rest : (Opt P2-Word)])
  #:transparent)

(define-struct P2-Extension
  ([field : String])
  #:transparent)

(define-struct P2-Restriction
  ([field : String])
  #:transparent)

(define-struct P2-Selection
  ([nested : (Listof String)]
   [field : String])
  #:transparent)

(define-struct P2-Update
  ([nested : (Listof String)]
   [field : String])
  #:transparent)



(define-struct P2-Variant-Lit
  ([field : String]
   [val : P2-Word])
  #:transparent)

(define-struct P2-Embed
  ([field : String])
  #:transparent)

(define-struct P2-Case
  ([branches : (Listof P2-Case-Branch)]
   [otherwise : (Listof P2-Word)])
  #:transparent)

(define-struct P2-Case-Branch
  ([var : String]
   [body : (Listof P2-Word)])
  #:transparent)



(define-struct P2-Bag-Lit
  ([elems : (Listof P2-Word)]
   [rest : (Opt P2-Word)])
  #:transparent)

(define-struct P2-Bag-Get
  ([type : P2-Type])
  #:transparent)

(define-struct P2-Bag-Put ()
  #:transparent)

(define-struct P2-Bag-Drop
  ([type : P2-Type])
  #:transparent)



(define-struct P2-Union-Lit
  ([elem : P2-Word])
  #:transparent)

(define-struct P2-Typeof
  ([branches : (Listof P2-Typeof-Branch)]
   [otherwise : (Listof P2-Word)])
  #:transparent)

(define-struct P2-Typeof-Branch
  ([type : P2-Type]
   [body : (Listof P2-Word)])
  #:transparent)



(define-struct P2-With-State
  ([body : P2-Block])
  #:transparent)

(define-struct P2-Newref ()
  #:transparent)
(define-struct P2-Getref ()
  #:transparent)
(define-struct P2-Putref ()
  #:transparent)



(define-struct P2-Untag
  ([name : String]
   [alias : P2-Import-Path])
  #:transparent)



(define-struct P2-Do ()
  #:transparent)



(define-type P2-Pattern
  (U P2-Named-Pat
     P2-Ref-Pat
     P2-Cons-Pat
     P2-Wildcard-Pat
     P2-Tuple-Pat
     P2-List-Pat
     P2-Vector-Pat
     P2-Slice-Pat
     P2-Record-Pat
     P2-Dict-Pat
     String
     Char
     Integer
     Flonum))
(provide P2-Pattern)

(define-struct P2-Named-Pat
  ([name : String]
   [pattern : P2-Pattern])
  #:transparent)

(define-struct P2-Ref-Pat
  ([pattern : P2-Pattern])
  #:transparent)

(define-struct P2-Cons-Pat
  ([constructor : String]
   [alias : P2-Import-Path]
   [elems : (Listof P2-Pattern)])
  #:transparent)

(define-struct P2-Wildcard-Pat ()
  #:transparent)

(define-struct P2-Tuple-Pat
  ([elems : (Listof P2-Pattern)]
   [dotted : (Opt P2-Pattern)])
  #:transparent)

(define-struct P2-List-Pat
  ([left : (Listof P2-Pattern)]
   [right : (Listof P2-Pattern)]
   [dotted : (Opt P2-Pattern)])
  #:transparent)

(define-struct P2-Vector-Pat
  ([left : (Listof P2-Pattern)]
   [right : (Listof P2-Pattern)]
   [dotted : (Opt P2-Pattern)])
  #:transparent)

(define-struct P2-Slice-Pat
  ([left : (Listof P2-Pattern)]
   [right : (Listof P2-Pattern)]
   [dotted : (Opt P2-Pattern)])
  #:transparent)

(define-struct P2-Record-Pat
  ([fields : (Listof (Pair String P2-Pattern))]
   [rest : (Opt String)])
  #:transparent)

(define-struct P2-Dict-Pat
  ([fields : (Listof (Pair P2-Pattern P2-Pattern))]
   [rest : (Opt String)])
  #:transparent)



(define-struct P2-Fixed-Term
  ([terms : (Listof (U Integer String (Pair Integer String)))])
  #:transparent)
(provide (struct-out P2-Fixed-Term))

(define-struct P2-Operator
  ([name : String])
  #:transparent)
(provide (struct-out P2-Operator))

(define-struct P2-Term-Constructor
  ([name : String]
   [alias : P2-Import-Path]
   [fixed : (Opt P2-Fixed-Term)])
  #:transparent)
(provide (struct-out P2-Term-Constructor))

(define-struct P2-Module-Variable
  ([name : String]
   [alias : P2-Import-Path]
   [fixed : (Opt P2-Fixed-Term)])
  #:transparent)
(provide (struct-out P2-Module-Variable))

(define-struct P2-Term-Variable
  ([name : String]
   [fixed : (Opt P2-Fixed-Term)])
  #:transparent)
(provide (struct-out P2-Term-Variable))