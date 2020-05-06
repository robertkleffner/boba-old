#lang typed/racket

(define-struct None () #:transparent)
(define-struct (A) Some ([v : A]) #:transparent)
(provide (struct-out None) (struct-out Some))
(define-type (Opt A) (U None (Some A)))



(define-type P1-Boba-Program
  (Pair P1-Boba-Main (Immutable-HashTable (U String P1-Remote-Path) P1-Boba-Unit)))
(provide P1-Boba-Program)

(define-type P1-Boba-Units
  (Immutable-HashTable (U String P1-Remote-Path) P1-Boba-Unit))
(provide P1-Boba-Units)

(define-struct P1-Boba-Unit
  ([imports : (Listof P1-Import)]
   [declarations : (Listof P1-Declaration)]
   [exports : (Listof String)])
  #:transparent)
(provide (struct-out P1-Boba-Unit))

(define-struct P1-Boba-Main
  ([imports : (Listof P1-Import)]
   [declarations : (Listof P1-Declaration)]
   [main : (Listof P1-Word)])
  #:transparent)
(provide (struct-out P1-Boba-Main))



(define-type P1-Import-Path (U String P1-Remote-Path))
(provide P1-Import-Path)

(: boba-path->string (-> P1-Import-Path String))
(define (boba-path->string path)
  (match path
    [(P1-Remote-Path org proj name major minor patch)
     (string-append org "." proj "." name ":" (number->string major) "." (number->string minor) "." (number->string patch))]
    [(? string? x) x]))
(provide boba-path->string)

(define-struct P1-Remote-Path
  ([org : String]
   [proj : String]
   [name : String]
   [major : Integer]
   [minor : Integer]
   [patch : Integer])
  #:transparent)
(provide (struct-out P1-Remote-Path))

(define-struct P1-Import
  ([names : (Listof String)]
   [path : P1-Import-Path]
   [alias : String])
  #:transparent)
(provide (struct-out P1-Import))



(define-type P1-Declaration
  (U P1-Data
     P1-Pattern-Synonym
     P1-Adhoc
     P1-Overload
     P1-Derive
     P1-Tag
     P1-Type-Synonym
     P1-Effect-Synonym
     P1-Predicate-Synonym
     P1-Check
     P1-Function
     P1-Recursive
     P1-Test
     P1-Law))
(provide P1-Declaration)



(define-struct P1-Data
  ([name : String]
   [tag : (Opt String)]
   [params : (Listof String)]
   [constructors : (Listof P1-Constructor)])
  #:transparent)
(provide (struct-out P1-Data))

(define-struct P1-Constructor
  ([name : String]
   [elems : (Listof P1-Type)])
  #:transparent)
(provide (struct-out P1-Constructor))



(define-struct P1-Pattern-Synonym
  ([name : String]
   [params : (Listof String)]
   [pattern : P1-Pattern])
  #:transparent)
(provide (struct-out P1-Pattern-Synonym))



(define-struct P1-Adhoc
  ([name : String]
   [predicate-name : String]
   [qualified-var : String]
   [type : P1-Type])
  #:transparent)
(provide (struct-out P1-Adhoc))

(define-struct P1-Overload
  ([name : String]
   [instance : P1-Type]
   [context : (Listof P1-Type)]
   [body : (Listof P1-Word)])
  #:transparent)
(provide (struct-out P1-Overload))

(define-struct P1-Derive
  ([name : String]
   [instance : P1-Type]
   [context : (Listof P1-Type)])
  #:transparent)
(provide (struct-out P1-Derive))



(define-struct P1-Tag
  ([name : String]
   [tag : String])
  #:transparent)
(provide (struct-out P1-Tag))



(define-struct P1-Type-Synonym
  ([name : String]
   [params : (Listof String)]
   [type : P1-Type])
  #:transparent)
(provide (struct-out P1-Type-Synonym))

(define-struct P1-Effect-Synonym
  ([name : String]
   [params : (Listof String)]
   [effects : (Listof P1-Effect)])
  #:transparent)
(provide (struct-out P1-Effect-Synonym))

(define-struct P1-Predicate-Synonym
  ([name : String]
   [param : String]
   [predicates : (Listof P1-Predicate)])
  #:transparent)
(provide (struct-out P1-Predicate-Synonym))



(define-struct P1-Check
  ([name : String]
   [type : P1-Type])
  #:transparent)
(provide (struct-out P1-Check))

(define-struct P1-Function
  ([name : String]
   [fixed : (Listof String)]
   [body : (Listof P1-Word)])
  #:transparent)
(provide (struct-out P1-Function))

(define-struct P1-Recursive
  ([mutual : (Listof P1-Function)])
  #:transparent)
(provide (struct-out P1-Recursive))



(define-struct P1-Test
  ([name : String]
   [left : (Listof P1-Word)]
   [right : (Listof P1-Word)]
   [type : String]
   [compare : (Opt P1-Block)])
  #:transparent)
(provide (struct-out P1-Test))

(define-struct P1-Law
  ([name : String]
   [exhaustive : Boolean]
   [left : (Listof P1-Word)]
   [right : (Listof P1-Word)]
   [type : String]
   [compare : (Opt P1-Block)])
  #:transparent)
(provide (struct-out P1-Law))



(define-struct P1-Predicate
  ([name : String]
   [alias : (Opt String)]
   [type : P1-Type]
   [dotted : Boolean])
  #:transparent)
(provide (struct-out P1-Predicate))

(define-type P1-Type
  (U P1-Type-App
     P1-Tag-Type
     P1-Fixed-Size-Type
     P1-Type-Seq
     P1-Field-Type
     P1-Effect-Seq
     P1-Abelian-Var
     P1-Abelian-Cons
     P1-Type-Cons
     P1-Type-Var))
(provide P1-Type)

(define-struct P1-Type-App
  ([left : P1-Type]
   [right : P1-Type])
  #:transparent)
(provide P1-Type-App)

(define-struct P1-Tag-Type
  ([variables : (Listof P1-Abelian-Var)]
   [constructor : (Listof P1-Abelian-Cons)])
  #:transparent)

(define-struct P1-Fixed-Size-Type
  ([variables : (Listof P1-Abelian-Var)]
   [constant : Integer])
  #:transparent)

(define-struct P1-Type-Seq
  ([elems : (Listof P1-Type)]
   [dotted : (Opt P1-Type)])
  #:transparent)

(define-struct P1-Field-Seq
  ([fields : (Listof P1-Field-Type)]
   [dotted : String])
  #:transparent)

(define-struct P1-Field-Type
  ([field : String]
   [type : P1-Type])
  #:transparent)

(define-struct P1-Effect-Seq
  ([effects : (Listof P1-Effect)]
   [dotted : String])
  #:transparent)

(define-struct P1-Effect
  ([op : String]
   [params : (Listof P1-Type)])
  #:transparent)

(define-struct P1-Abelian-Var
  ([name : String]
   [exponent : Integer])
  #:transparent)

(define-struct P1-Abelian-Cons
  ([name : String]
   [alias : (Opt String)]
   [exponent : Integer])
  #:transparent)

(define-struct P1-Type-Cons
  ([name : String]
   [alias : (Opt String)])
  #:transparent)

(define-struct P1-Type-Var
  ([name : String])
  #:transparent)



(define-type P1-Word
  (U P1-Block
     P1-Local
     P1-Local-Mutual
     P1-Handle
     P1-Match
     P1-If
     P1-While
     P1-Until
     P1-For-Comp
     P1-Anonymous
     P1-Tuple-Lit
     P1-List-Lit
     P1-Vector-Lit
     P1-Slice-Lit
     P1-Dict-Lit
     P1-Record-Lit
     P1-Extension
     P1-Restriction
     P1-Selection
     P1-Update
     P1-Variant-Lit
     P1-Embed
     P1-Case
     P1-Bag-Lit
     P1-Bag-Get
     P1-Bag-Put
     P1-Bag-Drop
     P1-Union-Lit
     P1-Typeof
     P1-With-State
     P1-Newref
     P1-Getref
     P1-Putref
     P1-Untag
     P1-Term-Constructor
     P1-Term-Variable
     P1-Operator
     P1-Do
     String
     Char
     Integer
     Flonum))
     


(define-struct P1-Block
  ([statements : (Listof P1-Statement)])
  #:transparent)

(define-type P1-Statement (U P1-Let-Statement (Listof P1-Word)))

(define-struct P1-Let-Statement
  ([patterns : (Listof P1-Pattern)]
   [body : (Listof P1-Word)])
  #:transparent)



(define-struct P1-Local
  ([name : String]
   [body : (Listof P1-Word)]
   [in : P1-Word])
  #:transparent)

(define-struct P1-Local-Mutual
  ([mutuals : (Pair String (Listof P1-Word))]
   [in : P1-Word])
  #:transparent)



(define-struct P1-Handle
  ([params : (Listof String)]
   [body : P1-Block]
   [handlers : (Listof P1-Handler)]
   [return : (Listof P1-Word)])
  #:transparent)

(define-struct P1-Handler
  ([op : String]
   [params : (Listof String)]
   [body : (Listof P1-Word)])
  #:transparent)



(define-struct P1-Match
  ([branches : (U (Listof P1-Rigid-Branch) (Listof P1-Flex-Branch))]
   [otherwise : (Listof P1-Word)])
  #:transparent)

(define-struct P1-Rigid-Branch
  ([patterns : (Listof P1-Pattern)]
   [body : (Listof P1-Word)])
  #:transparent)

(define-struct P1-Flex-Branch
  ([patterns : (Listof P1-Pattern)]
   [dotted : P1-Pattern]
   [body : (Listof P1-Word)])
  #:transparent)



(define-struct P1-If
  ([condition : P1-Block]
   [then : P1-Block]
   [else : P1-Block])
  #:transparent)

(define-struct P1-While
  ([condition : P1-Block]
   [body : P1-Block])
  #:transparent)

(define-struct P1-Until
  ([condition : P1-Block]
   [body : P1-Block])
  #:transparent)



(define-struct P1-For-Comp
  ([kind : String]
   [fors : (Listof P1-For-Clause)]
   [breaks : (Listof P1-Break-Clause)]
   [body : P1-Block])
  #:transparent)

(define-type P1-For-Clause
  (U P1-For-Select
     P1-For-When
     P1-For-Length
     P1-For-Result
     P1-For-Break
     P1-For-Final))

(define-struct P1-For-Select
  ([name : String]
   [body : (Listof P1-Word)])
  #:transparent)

(define-struct P1-For-When
  ([body : P1-Statement]
   [negated : Boolean])
  #:transparent)

(define-struct P1-For-Length
  ([size : P1-Fixed-Term]
   [fill : (Listof P1-Word)])
  #:transparent)

(define-struct P1-For-Result
  ([result : (Listof P1-Word)])
  #:transparent)

(define-type P1-Break-Clause
  (U P1-For-Break
     P1-For-Final))

(define-struct P1-For-Break
  ([break : (Listof P1-Word)])
  #:transparent)

(define-struct P1-For-Final
  ([break : (Listof P1-Word)])
  #:transparent)



(define-struct P1-Anonymous
  ([body : (Listof P1-Word)])
  #:transparent)



(define-struct P1-Tuple-Lit
  ([elems : (Listof P1-Word)]
   [rest : (Opt P1-Word)])
  #:transparent)

(define-struct P1-List-Lit
  ([left : (Listof P1-Word)]
   [right : (Listof P1-Word)]
   [rest : (Opt P1-Word)])
  #:transparent)

(define-struct P1-Vector-Lit
  ([left : (Listof P1-Word)]
   [right : (Listof P1-Word)]
   [rest : (Opt P1-Word)])
  #:transparent)

(define-struct P1-Slice-Lit
  ([subslice : (Opt P1-Slice-Lit)]
   [start : P1-Word]
   [end : P1-Word])
  #:transparent)

(define-struct P1-Dict-Lit
  ([fields : (Listof (Pair P1-Word P1-Word))]
   [rest : (Opt P1-Word)])
  #:transparent)



(define-struct P1-Record-Lit
  ([fields : (Listof (Pair String P1-Word))]
   [rest : (Opt P1-Word)])
  #:transparent)

(define-struct P1-Extension
  ([field : String])
  #:transparent)

(define-struct P1-Restriction
  ([field : String])
  #:transparent)

(define-struct P1-Selection
  ([nested : (Listof String)]
   [field : String])
  #:transparent)

(define-struct P1-Update
  ([nested : (Listof String)]
   [field : String])
  #:transparent)



(define-struct P1-Variant-Lit
  ([field : String]
   [val : P1-Word])
  #:transparent)

(define-struct P1-Embed
  ([field : String])
  #:transparent)

(define-struct P1-Case
  ([branches : (Listof P1-Case-Branch)]
   [otherwise : (Listof P1-Word)])
  #:transparent)

(define-struct P1-Case-Branch
  ([var : String]
   [body : (Listof P1-Word)])
  #:transparent)



(define-struct P1-Bag-Lit
  ([elems : (Listof P1-Word)]
   [rest : (Opt P1-Word)])
  #:transparent)

(define-struct P1-Bag-Get
  ([type : P1-Type])
  #:transparent)

(define-struct P1-Bag-Put ()
  #:transparent)

(define-struct P1-Bag-Drop
  ([type : P1-Type])
  #:transparent)



(define-struct P1-Union-Lit
  ([elem : P1-Word])
  #:transparent)

(define-struct P1-Typeof
  ([branches : (Listof P1-Typeof-Branch)]
   [otherwise : (Listof P1-Word)])
  #:transparent)

(define-struct P1-Typeof-Branch
  ([type : P1-Type]
   [body : (Listof P1-Word)])
  #:transparent)



(define-struct P1-With-State
  ([body : P1-Block])
  #:transparent)

(define-struct P1-Newref ()
  #:transparent)
(define-struct P1-Getref ()
  #:transparent)
(define-struct P1-Putref ()
  #:transparent)



(define-struct P1-Untag
  ([name : String])
  #:transparent)



(define-struct P1-Do ()
  #:transparent)



(define-type P1-Pattern
  (U P1-Named-Pat
     P1-Ref-Pat
     P1-Cons-Pat
     P1-Wildcard-Pat
     P1-Tuple-Pat
     P1-List-Pat
     P1-Vector-Pat
     P1-Slice-Pat
     P1-Record-Pat
     P1-Dict-Pat
     String
     Char
     Integer
     Flonum))

(define-struct P1-Named-Pat
  ([name : String]
   [pattern : P1-Pattern])
  #:transparent)

(define-struct P1-Ref-Pat
  ([pattern : P1-Pattern])
  #:transparent)

(define-struct P1-Cons-Pat
  ([constructor : String]
   [alias : (Opt String)]
   [elems : (Listof P1-Pattern)])
  #:transparent)

(define-struct P1-Wildcard-Pat ()
  #:transparent)

(define-struct P1-Tuple-Pat
  ([elems : (Listof P1-Pattern)]
   [dotted : (Opt P1-Pattern)])
  #:transparent)

(define-struct P1-List-Pat
  ([left : (Listof P1-Pattern)]
   [right : (Listof P1-Pattern)]
   [dotted : (Opt P1-Pattern)])
  #:transparent)

(define-struct P1-Vector-Pat
  ([left : (Listof P1-Pattern)]
   [right : (Listof P1-Pattern)]
   [dotted : (Opt P1-Pattern)])
  #:transparent)

(define-struct P1-Slice-Pat
  ([left : (Listof P1-Pattern)]
   [right : (Listof P1-Pattern)]
   [dotted : (Opt P1-Pattern)])
  #:transparent)

(define-struct P1-Record-Pat
  ([fields : (Listof (Pair String P1-Pattern))]
   [rest : (Opt String)])
  #:transparent)

(define-struct P1-Dict-Pat
  ([fields : (Listof (Pair P1-Pattern P1-Pattern))]
   [rest : (Opt String)])
  #:transparent)



(define-struct P1-Fixed-Term
  ([terms : (Listof (U Integer String (Pair Integer String)))])
  #:transparent)
(provide (struct-out P1-Fixed-Term))

(define-struct P1-Operator
  ([name : String])
  #:transparent)
(provide (struct-out P1-Operator))

(define-struct P1-Term-Constructor
  ([name : String]
   [alias : (Opt String)]
   [fixed : (Opt P1-Fixed-Term)])
  #:transparent)
(provide (struct-out P1-Term-Constructor))

(define-struct P1-Term-Variable
  ([name : String]
   [alias : (Opt String)]
   [fixed : (Opt P1-Fixed-Term)])
  #:transparent)
(provide (struct-out P1-Term-Variable))