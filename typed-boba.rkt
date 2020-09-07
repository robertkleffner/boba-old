#lang algebraic/racket/base

(require algebraic/racket/base/forms)
(require racket/list)
(require racket/hash)
(require racket/set)
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

(define seq-kind-arg
  (function
   [(K-Seq arg) arg]
   [a (error "Kind error: expected a sequence kind")]))

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
       T-Dot

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

;; instantiate-scheme : Boba-Type-Scheme, Context, FreshVarStream -> Boba-Qual-Type, Context
(define (instantiate-scheme scheme context fresh)
  (for/fold ([newcontext context]
             [newtype (Boba-Type-Scheme-body scheme)])
            ([quant (Boba-Type-Scheme-quantified scheme)])
    (define var (fresh "tc"))
    (values (cons (TypeVarIntro var (cdr quant)) newcontext)
            (qual-substitute (hash (car quant) (T-Var var (cdr quant)))))))
  


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
       SeqConstraint ; [Context-Entry] [Boba-Type] [Boba-Type]
       EffConstraint ; [Context-Entry] [Boba-Type] [Boba-Type]
       FieldConstraint ; [Context-Entry] [Boba-Type] [Boba-Type]
       ))

;; constraint? : Context-Entry -> Bool
(define constraint?
  (function
   [(TypeConstraint l r) #true]
   [(FlexRigidHullConstraint d l r) #true]
   [(AbelianConstraint d e) #true]
   [(SeqConstraint l r) #true]
   [(EffConstraint l r) #true]
   [(FieldConstraint l r) #true]
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
   [(T-Dot)
    (K-Arr (K-Val) (K-Val))]
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

(define (abelian-type? type)
  (define k (kind type))
  (or (equal? k (K-Tag)) (equal? k (K-Size))))

(define dotted-type?
  (function
   [(T-App (T-Dot) t) #true]
   [other #false]))

;; make-hull : Boba-Type, FreshVarStream -> Boba-Type, [(String, Boba-Kind)], [(String, Boba-Type)]
(define (make-hull type fresh)
  (define f
    (function
     [(T-App left right)
      (define l-kind (kind left))
      (define-values (lh ld lc) (make-hull left fresh))
      (cond
        [(abelian-ctor? l-kind)
         (define f (fresh "h"))
         (values (T-App lh (T-Var f))
                 (append ld (list (cons f arr-kind-arg l-kind)))
                 (append lc (list (cons f right))))]
        [else
         (define-values (rh rd rc) (make-hull right fresh))
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
          (define-values (h d c) (make-hull t fresh))
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



;; type-substitute : { String -> Boba-Type }, Boba-Type -> Boba-Type
;; Performs substitution over the input type for each variable-substitute pair
;; in the given substitution.
(define type-substitute
  (function*
   [(subst (T-Var name))
    (if (hash-has-key? subst name)
        (hash-ref subst name)
        (T-Var name))]
   ; special case for dotted types, which can be eliminated by substituting with a sequence
   [(subst (T-App (T-Dot) t-dotted))
    (case (type-substitute subst t-dotted)
      [(T-Seq ts) (T-Seq ts)]
      [(T-App (T-Dot) t-subdot) (T-App (T-Dot) t-subdot)]
      [t-sub (T-App (T-Dot) t-sub)])]
   [(subst (T-App left right))
    (push-sequence-subs-up T-App (type-substitute subst left) (type-substitute subst right))]
   ; sequences flatten sub-sequences substituted for dotted types
   [(subst (T-Seq ts))
    (T-Seq (for/fold ([new-ts null])
                     ([sub-t (for/list ([t ts]) (type-substitute subst t))])
             (case sub-t
               [(T-Seq sub-ts) (append new-ts sub-ts)]
               [else-t (append new-ts (list sub-t))])))]
   [(subst (T-Mul left right))
    (push-sequence-subs-up T-Mul (type-substitute subst left) (type-substitute subst right))]
   [(subst (T-Pow exp body))
    (case (type-substitute subst body)
      [(T-Seq bs) (T-Seq (for/list ([b bs]) (T-Pow exp b)))]
      [b (T-Pow exp b)])]
   [(subst type) type]))

;; push-sequence-subs-up : (Boba-Type, Boba-Type -> Boba-Type), Boba-Type, Boba-Type -> Boba-Type
;; Sequence types need to rise to the outer layer as far as possible during substitution.
;; This helper method pushes them up through binary parts of the type structure.
(define (push-sequence-subs-up ctor lefts rights)
  (case-values
   (values lefts rights)
   [((T-Seq ls) (T-Seq rs))
    (if (equal? (length ls) (length rs))
        (T-Seq (for/list ([l ls] [r rs]) (ctor l r)))
        (error "Sequence variables in type application were substituted with sequences of differing lengths."))]
   [((T-Seq ls) r)
    (T-Seq (for/list ([l ls]) (ctor l r)))]
   [(l (T-Seq rs))
    (T-Seq (for/list ([r rs]) (ctor l r)))]
   [(l r)
    (ctor l r)]))

;; pred-substitute : { String -> Boba-Type }, Boba-Pred -> Boba-Pred
(define (pred-substitute subst pred)
  (Boba-Pred (Boba-Pred-name pred)
             (type-substitute subst (Boba-Pred-arg pred))))

;; qual-substitute : { String -> Boba-Type }, Boba-Qual-Type -> Boba-Qual-Type
(define (qual-substitute subst qual)
  (Boba-Qual-Type (for/list ([p (Boba-Qual-Type-context qual)]) (pred-substitute subst p))
                  (type-substitute subst (Boba-Qual-Type-head qual))))

;; compose-substitutions : { String -> Boba-Type }, { String -> Boba-Type } -> { String -> Boba-Type }
;; Generates a new substitution that when applied to a type is equivalent to
;; applying the first substitution after applying the second substitution.
(define (compose-substitutions s1 s2)
  (define s2new (for/hash ([(v sub) (in-hash s2)])
                  (values v (type-substitute s1 sub))))
  (hash-union s1 s2new #:combine (λ (l r) l)))

;; type-free-vars : Boba-Type -> Set String
(define type-free-vars
  (function
   [(T-Var name) (set name)]
   [(T-App left right) (set-union (type-free-vars left) (type-free-vars right))]
   [(T-Seq types) (apply set-union (map type-free-vars types))]
   [(T-Mul left right) (set-union (type-free-vars left) (type-free-vars right))]
   [(T-Pow exp body) (type-free-vars body)]
   [a (set)]))

;; type-unify : Boba-Type, Boba-Type -> { String -> Boba-Type }
;; TODO: several important parts of unification (sequence, row, abelian) remain unimplemented for now
(define type-unify
  (function*
   [((T-Var left) (T-Var right))
    (if (string=? left right)
        (hash)
        (hash left (T-Var right)))]
   [((T-Var left) right)
    (if (set-member? (type-free-vars right) left)
        (error "failed occurs check in type unification")
        (hash left right))]
   [(left (T-Var right))
    (if (set-member? (type-free-vars left) right)
        (error "failed occurs check in type unification")
        (hash right left))]
   [((T-App l1 r1) (T-App l2 r2))
    (define s1 (type-unify l1 l2))
    (define s2 (type-unify (type-substitute s1 r1)
                           (type-substitute s1 r2)))
    (compose-substitutions s2 s1)]
   [((T-Mul l1 r1) (T-Mul l2 r2))
    (define s1 (type-unify l1 l2))
    (define s2 (type-unify (type-substitute s1 r1)
                           (type-substitute s1 r2)))
    (compose-substitutions s2 s1)]
   [((T-Seq t1s) (T-Seq t2s))
    (for/fold ([sub (hash)])
              ([t1 t1s]
               [t2 t2s])
      (compose-substitutions
       (type-unify (type-substitute sub t1)
                   (type-substitute sub t2))
       sub))]
   [(l r)
    (if (equal? l r)
        (hash)
        (error "non-equal types encountered during unification."))]))



;; solve-constraint : Constraint, FreshVarStream, ConstraintPrefix, ConstraintSuffix -> Context
(define solve-constraint
  (function*
   [((TypeConstraint deps left right) fresh prefix suffix)
    (case-values
     (values left right)

     ; unify two applications
     [((T-App l1 r1) (T-App l2 r2))
      (append prefix (list (TypeConstraint l1 l2)) (list (TypeConstraint r1 r2)) suffix)]

     ; unify two primitives
     [((T-Prim n1) (T-Prim n2))
      (if (equal? n1 n2)
          (append prefix suffix)
          (error "Primitive types could not be unified"))]

     ; unify two constructors
     [((T-Con n1 k1) (T-Con n2 k2))
      (if (and (equal? n1 n2) (equal? k1 k2))
          (append prefix suffix)
          (error "Type constructors could not be unified"))]

     ; unify two sequences
     [((T-Seq s1) (T-Seq s2))
      (define k1 (seq-kind-arg (kind (T-Seq s1))))
      (define k2 (seq-kind-arg (kind (T-Seq s2))))
      (when (not (equal? k1 k2))
        (error "Sequence types must have same kind to be unified."))
      (cond
        [(equal? k1 (K-Val)) (append prefix (list (SeqConstraint s1 s2)) suffix)]
        [(equal? k1 (K-Eff)) (append prefix (list (EffConstraint s1 s2)) suffix)]
        [(equal? k1 (K-Field)) (append prefix (list (FieldConstraint s1 s2)) suffix)]
        [else (error "Encountered a sequence of kinds that I don't know how to unify.")])]

     ; variable matched with variable
     [((T-Var l-name l-kind) (T-Var r-name r-kind))
      (define top (last prefix))
      (define popped (drop-right prefix 1))
      (when (not (equal? l-kind r-kind))
        (error "Kinds must be equal when unifying two type variables."))
      (case top
        [(LocalityMark)
         (append prefix (list (TypeConstraint (T-Var l-name) (T-Var r-name)) top) suffix)]
        [(TermVarBind name scheme)
         (append prefix (list (TypeConstraint (T-Var l-name) (T-Var r-name)) top) suffix)]
        [(TypeVarIntro name kind)
         (case-values
          (values (equal? name l-name) (equal? name r-name))
          [(#true #true) (append popped (list top) suffix)]
          [(#true #false) (append popped (list (TypeVarDef name (T-Var r-name r-kind)) top) suffix)]
          [(#false #true) (append popped (list (TypeVarDef name (T-Var l-name l-kind)) top) suffix)]
          [(#false #false) (append popped (list (TypeConstraint (T-Var l-name l-kind) (T-Var r-name r-kind)) top) suffix)])]
        [(TypeVarDef name type)
         (case-values
          (values (equal? name l-name) (equal? name r-name))
          [(#true #true) (append popped (list top) suffix)]
          [(e1 e2)
           (define subst (hash name type))
           (define constraint (TypeConstraint (type-substitute subst (T-Var l-name))
                                              (type-substitute subst (T-Var r-name))))
           (append popped (list constraint top) suffix)])]
        [a (error "Unknown context entry when unifying variables")])]

     ; variable matched with type fragment
     [((T-Var l-name l-kind) right)
      (append prefix (make-hull-constraint l-name right fresh) suffix)]
     [(left (T-Var r-name r-kind))
      (append prefix (make-hull-constraint r-name left fresh) suffix)]

     ; unify two abelian types
     [(l-abel r-abel)
      #:if (and (abelian-type? l-abel) (abelian-type? r-abel))
      (define k1 (kind l-abel))
      (define k2 (kind r-abel))
      (when (not (equal? k1 k2))
        (error "Abelian types must have the same kind to be unified."))
      (cond
        [(equal? k1 (K-Tag))
         (append prefix (list (make-tag-constraint l-abel r-abel)) suffix)]
        [(equal? k1 (K-Size))
         (append prefix (list (make-size-constraint l-abel r-abel)) suffix)])]

     ; mis-matched type ast spine
     [(l r)
      (error "Type structure (rigid-rigid) mis-match")])]
   
   [((FlexRigidHullConstraint deps left right) fresh prefix suffix)
    ; todo
    null]
   
   [((AbelianConstraint dep equation) fresh prefix suffix)
    ; todo
    null]

   [((SeqConstraint ls rs) fresh prefix suffix)
    (if (and (null? ls) (null? rs))
        (append prefix suffix)
        (case-values
         (values ls rs)
         [(((T-App (T-Dot) t-dotl)) ((T-App (T-Dot) t-dotr)))
          (append prefix (list (TypeConstraint t-dotl t-dotr)) suffix)]
         [(((T-App (T-Dot) t-dotted)) t-rest)
          (append prefix (expand-dots t-dotted t-rest fresh) suffix)]
         [(t-rest ((T-App (T-Dot) t-dotted)))
          (append prefix (expand-dots t-dotted t-rest fresh) suffix)]
         [((tl . tl-rest) (tr . tr-rest))
          (append prefix (list (TypeConstraint tl tr) (SeqConstraint ls rs)) suffix)]
         [(a b)
          (error "Mismatched sequence constraint lengths.")]))]

   [((EffConstraint ls rs) fresh prefix suffix)
    ; todo
    null]
   
   [((FieldConstraint ls rs) fresh prefix suffx)
    ; todo
    null]))

;; make-hull-constraint : String, Boba-Type, FreshVarStream -> Context
(define (make-hull-constraint flex rigid fresh)
  (define-values (hull deps defs) (make-hull rigid fresh))
  (define intros (for/list ([d deps]) (TypeVarIntro (car d) (cdr d))))
  (define constraints
    (for/list ([d defs])
      (case (kind (cdr d))
        [(K-Tag)
         (make-tag-constraint (T-Var (car d) (K-Tag)) (cdr d))]
        [(K-Size)
         (make-size-constraint (T-Var (car d) (K-Size)) (cdr d))])))
  (append* (list (FlexRigidHullConstraint intros flex hull)) constraints))

;; expand-dots : Boba-Type [Boba-Type] FreshVarStream -> Context
(define (expand-dots dotted rest fresh)
  (define vars (type-free-vars dotted))
  (define freshened
    (for/list ([v vars])
      (cons v
            (for/list ([r rest])
              (cons (fresh v) (dotted-type? r))))))
  (define subst
    (for/hash ([f freshened])
      (hash (car f)
            (T-Seq (for/list ([v (cdr f)])
                     (if (cdr v)
                         (T-App (T-Dot) (T-Var (car v) (K-Val)))
                         (T-Var (car v) (K-Val))))))))
  (define intros
    (append*
     (for/list ([f freshened])
       (for/list ([v (cdr f)])
         (TypeVarIntro v (K-Val))))))
  
  (append intros (type-substitute subst dotted)))

;; make-tag-constraint : Boba-Type, Boba-Type -> Constraint
(define (make-tag-constraint l r)
  (abelian-sub (type->tag-equation l) (type->tag-equation r)))

;; make-size-constraint : Boba-Type, Boba-Type -> Constraint
(define (make-size-constraint l r)
  (abelian-sub (type->size-equation l) (type->size-equation r)))

;; type->tag-equation : Boba-Type -> AbelianEquation
(define type->tag-equation
  (function
   [(T-Var name)
    (TagEquation (hash name 1) (hash))]
   [(T-Tag name)
    (TagEquation (hash) (hash name 1))]
   [(T-TagId)
    (TagEquation (hash) (hash))]
   [(T-Mul l r)
    (abelian-add (type->tag-equation l) (type->tag-equation r))]
   [(T-Pow exp t)
    (abelian-scale (type->tag-equation t) exp)]))

;; type->size-equation : Boba-Type -> AbelianEquation
(define type->size-equation
  (function
   [(T-Var name)
    (SizeEquation (hash name 1) 0)]
   [(T-Size num)
    (SizeEquation (hash) num)]
   [(T-SizeId)
    (SizeEquation (hash) 0)]
   [(T-Mul l r)
    (abelian-add (type->size-equation l) (type->size-equation r))]
   [(T-Pow exp t)
    (abelian-scale (type->size-equation t) exp)]))



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
   

