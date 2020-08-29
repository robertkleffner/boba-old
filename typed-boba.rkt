#lang algebraic/racket/base

(require racket/list)
(require racket/hash)
(module+ test
  (require rackunit))



(data Boba-Kind
      (K-Val ; *
       K-Eff ; !
       K-Field ; @
       K-Tag ; &
       K-Size ; #
       K-Seq ; Boba-Kind
       K-Arr ; Boba-Kind Boba-Kind
       ))

(define abelian-ctor?
  (function
   [(K-Arr (K-Tag) rgt) #true]
   [(K-Arr (K-Size) rgt) #true]
   [a #false]))

(define arr-kind-arg
  (function
   [(K-Arr arg res) arg]
   [a (error "Kind error: expected an arrow kind")]))

(define PRIM-NUM-KIND (K-Arr (K-Tag) (K-Val)))

(define PRIM-TYPE-KINDS
  (hash "Bool" (K-Val)
        "Char" (K-Val)
        "I8" PRIM-NUM-KIND
        "U8" PRIM-NUM-KIND
        "I16" PRIM-NUM-KIND
        "U16" PRIM-NUM-KIND
        "I32" PRIM-NUM-KIND
        "U32" PRIM-NUM-KIND
        "I64" PRIM-NUM-KIND
        "U64" PRIM-NUM-KIND
        "F32" PRIM-NUM-KIND
        "F64" PRIM-NUM-KIND
        "ISize" PRIM-NUM-KIND
        "USize" PRIM-NUM-KIND
        
        "-->" (K-Arr (K-Seq (K-Eff)) (K-Arr (K-Seq (K-Val)) (K-Arr (K-Seq (K-Val)) (K-Val))))
        
        "Tuple" (K-Arr (K-Seq (K-Val)) (K-Val))
        "List" (K-Arr (K-Val) (K-Val))
        "Vector" (K-Arr (K-Size) (K-Arr (K-Val) (K-Val)))
        "Slice" (K-Arr (K-Size) (K-Arr (K-Val) (K-Val)))
        "Dict" (K-Arr (K-Val) (K-Arr (K-Val) (K-Val)))
        "Record" (K-Arr (K-Seq (K-Field)) (K-Val))
        "Variant" (K-Arr (K-Seq (K-Field)) (K-Val))
        
        "Ref" (K-Arr (K-Val) (K-Val))
        ))

(data Boba-Type
      (T-Prim ; String
       T-Var ; String Kind
       T-Con ; String Kind
       T-App ; Boba-Type Boba-Type
       T-Eff ; String Kind
       T-Field ; String
       T-Seq ; [Boba-Type]

       T-TagId
       T-SizeId
       T-Tag ; String
       T-Size ; Integer
       T-Mul ; Boba-Type Boba-Type
       T-Pow ; Integer Boba-Type
       ))

(struct Boba-Pred
  (name ; String
   arg ; Boba-Type
   ) #:transparent)

(struct Boba-Qual-Type
  (context ; [Boba-Pred]
   head ; Boba-Type
   ) #:transparent)

(struct Boba-Type-Scheme
  (quantified ; [(String, Boba-Kind)]
   body ; Boba-Qual-Type
   ) #:transparent)



;; type Context = [Context-Entry]
;; type ConstraintPrefix = Context
;; type ConstraintSuffix = Context
;; type Constraint = TypeConstraint | FlexRigidHullConstraint | AbelianConstraint

(data Context-Entry
      (TypeVarIntro ; String Boba-Kind
       TypeVarDef ; String Boba-Type
       TermVarBind ; String Boba-Type-Scheme
       LocalityMark
       TypeClassBind ; String [Boba-Type-Scheme]
       TypeCtorBind ; String Boba-Kind
       TypeConstraint ; [Context-Entry] Boba-Type Boba-Type
       FlexRigidHullConstraint ; [Context-Entry] String Boba-Type
       AbelianConstraint ; (Maybe String) AbelianEquation
       ))

;; constraint? : Context-Entry -> Bool
(define constraint?
  (function
   [(TypeConstraint d l r) #true]
   [(FlexRigidHullConstraint d l r) #true]
   [(AbelianConstraint d e) #true]
   [a #false]))

;; kind : Boba-Type -> Boba-Kind
(define kind
  (function
   [(T-Prim name)
    (hash-ref PRIM-TYPE-KINDS name)]
   [(T-Var name kind)
    kind]
   [(T-Con name kind)
    kind]
   [(T-App left right)
    (define kl (kind left))
    (define kr (kind right))
    (define-values (fun arg) ((phi (K-Arr fun arg) (values fun arg)) kl))
    (if (equal? fun kr)
        arg
        (error "Kind error: could not apply kind arrow to given kind arg"))]
   [(T-Eff name kind)
    kind]
   [(T-Field name)
    (K-Arr (K-Val) (K-Field))]
   [(T-Seq types)
    (define kinds (map kind types))
    (if (andmap (λ (k) (equal? k (first kinds))))
        (K-Seq (first kinds))
        (error "Kind error: all types within a type sequence must be the same kind"))]
   [(T-TagId)
    (K-Tag)]
   [(T-SizeId)
    (K-Size)]
   [(T-Tag name)
    (K-Tag)]
   [(T-Size size)
    (K-Size)]
   [(T-Mul left right)
    (define kl (kind left))
    (define kr (kind right))
    (if (equal? kl kr)
        kl
        (error "Kind error: both types in an abelian type conjunction must be the same kind"))]
   [(T-Pow exp type)
    (kind type)]))

;; make-hull : Boba-Type, FreshVarStream, Context -> Boba-Type, [(String, Boba-Kind)], [(String, Boba-Type)]
(define (make-hull type fresh ctx)
  (define f
    (function
     [(T-App left right)
      (define l-kind (kind left ctx))
      (define-values (lh ld lc) (make-hull left fresh ctx))
      (cond
        [(abelian-ctor? l-kind)
         (define f (fresh "h"))
         (values (T-App lh (T-Var f))
                 (append ld (list (cons f arr-kind-arg l-kind)))
                 (append lc (list (cons f right))))]
        [else
         (define-values (rh rd rc) (make-hull right fresh ctx))
         (values (T-App lh rh)
                 (append ld rd)
                 (append lc rc))])]
     [(T-Prim name)
      (values (T-Prim name) null null)]
     [(T-Var name)
      (values (T-Var name) null null)]
     [(T-Con name)
      (values (T-Con name) null null)]
     [(T-Eff name)
      (values (T-Eff name) null null)]
     [(T-Field name)
      (values (T-Field name) null null)]
     [(T-Seq types)
      (define-values (hulls deps defs)
        (for/fold ([hulls null] [deps null] [defs null])
                  ([t types])
          (define-values (h d c) (make-hull t fresh ctx))
          (values (cons h hulls) (cons d deps) (cons c defs))))
      (values (T-Seq hulls) deps defs)]
     [a (error "Type error: cannot generate a hull for an abelian type component.")]))
  (f type))

;; head-normal-form? : Boba-Type -> Bool
(define head-normal-form?
  (function
   [(T-Var name) #true]
   [(T-App left right) (head-normal-form? left)]
   [a #false]))

;; pred-head-normal-form? : Boba-Pred -> Bool
(define (pred-head-normal-form? pred)
  (head-normal-form? (Boba-Pred-arg pred)))

;; type-substitute : Boba-Type, String, Boba-Type -> Boba-Type
(define type-substitute
  (function*
   [((T-App left right) var sub)
    (T-App (type-substitute left var sub) (type-substitute right var sub))]
   [((T-Var name) var sub)
    (if (equal? name var) sub (T-Var name))]
   [((T-Seq types) var sub)
    (T-Seq (for/list ([t types]) (type-substitute t var sub)))]
   [((T-Mul left right) var sub)
    (T-Mul (type-substitute left var sub) (type-substitute right var sub))]
   [((T-Pow exp body) var sub)
    (T-Pow exp (type-substitute body var sub))]))



;; solve-constraint : Constraint, FreshVarStream, ConstraintPrefix, ConstraintSuffix -> Context
(define solve-constraint
  (function*
   [((TypeConstraint deps left right) fresh prefix suffix)
    null]
   [((FlexRigidHullConstraint deps left right) fresh prefix suffix)
    null]
   [((AbelianConstraint dep equation) fresh prefix suffix)
    null]))



(data AbelianEquation
      (TagEquation ; { String -> Integer } { String -> Integer }
       SizeEquation ; { String -> Integer } Integer
       ))

(define abelian-vars
  (function
   [(TagEquation vars consts) vars]
   [(SizeEquation vars const) vars]))

;; abelian-identity? : AbelianEquation -> Bool
;; Determines whether an Abelian equation is 1 (the identity).
(define abelian-identity?
  (function
   [(TagEquation vars consts) (and (hash-empty? vars) (hash-empty? consts))]
   [(SizeEquation vars const) #false]))

;; abelian-constant? : AbelianEquation -> Bool
;; Determines whether an Abelian equation is constant.
(define abelian-constant?
  (function
   [(TagEquation vars consts) (hash-empty? vars)]
   [(SizeEquation vars const) (hash-empty? vars)]))

;; abelian-exponent : AbelianEquation, String -> Integer
;; Gets the exponent part of the specified Abelian variable.
(define abelian-exponent
  (function*
   [((TagEquation vars consts) var) (hash-ref vars var)]
   [((SizeEquation vars const) var) (hash-ref vars var)]))

;; divides-powers : AbelianEquation, Integer -> Bool
;; Checks whether all exponents in the equation are integer multiples of the given divisor.
(define divides-powers
  (function*
   [((TagEquation vars consts) divisor)
    (define (divides item) (zero? (remainder exp divisor)))
    (and (andmap divides (hash-values vars)) (andmap divides (hash-values consts)))]
   [((SizeEquations var const) divisor)
    (error "Abelian eqn error: divides-powers not yet implemented for fixed size equations.")]))

;; abelian-var-not-max : AbelianEquation, String -> Bool
;; Checks whether the specified variable does not have the
;; highest exponent of the variables in the equation.
(define (abelian-var-not-max eqn var)
  (define vars (abelian-vars eqn))
  (define exp (hash-ref vars var))
  (for/or ([(v e) (in-hash vars)])
    (and (not (equal? v var)) (>= (abs v) exp))))

;; hash-map-values : { A -> B }, (B -> C) -> { A -> C }
(define (hash-map-values hash proc)
  (for/hash ([(k v) hash])
    (values k (proc v))))

;; abelian-invert : AbelianEquation -> AbelianEquation
;; Inverts the exponent of every component in the equation.
(define abelian-invert
  (function
   [(TagEquation vars consts)
    (TagEquation (hash-map-values vars -) (hash-map-values consts -))]
   [(SizeEquation vars const)
    (SizeEquation (hash-map-values vars -) (- const))]))

(define (combine-exponent-maps m1 m2)
  (for/hash ([(k v) (hash-union m1 m2 +)]
             #:when (not (zero? v)))
    (values k v)))

;; abelian-add : AbelianEquation, AbelianEquation -> AbelianEquation
(define abelian-add
  (function*
   [((TagEquation v1s c1s) (TagEquation v2s c2s))
    (TagEquation (combine-exponent-maps v1s v2s) (combine-exponent-maps c1s c2s))]
   [((SizeEquation v1s c1) (SizeEquation v2s c2))
    (SizeEquation (combine-exponent-maps v1s v2s) (+ c1 c2))]))

;; abelian-sub : AbelianEquation, AbelianEquation -> AbelianEquation
;; Subtracts eqn2 from eqn1. Equivalent to inverting eqn2 and
;; adding eqn1 to the inverted result.
(define (abelian-sub eqn1 eqn2) (abelian-add eqn1 (abelian-invert eqn2)))

;; abelian-scale : AbelianEquation, Integer -> AbelianEquation
(define abelian-scale
  (function*
   [((TagEquation vars consts) factor)
    (define (scale exp) (* exp factor))
    (TagEquation (hash-map-values vars scale) (hash-map-values consts scale))]
   [((SizeEquation vars const) factor)
    (define (scale exp) (* exp factor))
    (SizeEquation (hash-map-values vars scale) (scale const))]))

;; abelian-divide : AbelianEquation, Integer -> AbelianEquation
(define abelian-divide
  (function*
   [((TagEquation vars consts) divisor)
    (define (divide exp) (quotient exp divisor))
    (TagEquation (hash-map-values vars divide) (hash-map-values consts divide))]
   [((SizeEquation vars const) divisor)
    (define (divide exp) (quotient exp divisor))
    (SizeEquation (hash-map-values vars divide) (divide const))]))

;; abelian-pivot : AbelianEquation, String -> AbelianEquation
(define (abelian-pivot eqn var)
  (define exp (abelian-exponent eqn var))
  (define f
    (function
     [(TagEquation vars consts)
      (abelian-invert (abelian-divide (abelian-sub eqn (TagEquation (hash var exp) (hash))) exp))]
     [(SizeEquation vars const)
      (abelian-invert (abelian-divide (abelian-sub eqn (SizeEquation (hash var exp) 0))))]))
  (f eqn))

;; abelian-substitute : AbelianEquation, String, AbelianEquation -> AbelianEquation
(define (abelian-substitute eqn var sub)
  (define exp (abelian-exponent eqn var))
  (define f
    (function
     [(TagEquation vars consts)
      (abelian-add eqn (abelian-scale (abelian-sub sub (TagEquation (hash var exp) (hash))) exp))]
     [(SizeEquation vars const)
      (abelian-add eqn (abelian-scale (abelian-sub sub (SizeEquation (hash var exp) 0)) exp))]))
  (f eqn))
   

