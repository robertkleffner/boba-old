#lang racket/base

(struct p1-boba-unit (imports declarations export) #:transparent)

(struct p1-boba-main (imports declarations main) #:transparent)



(struct p1-import (names path alias) #:transparent)

(struct p1-remote-path (org proj name major minor patch) #:transparent)



(struct p1-data-decl (name params tag constructors) #:transparent)

(struct p1-constructor (name elems) #:transparent)



(struct p1-pattern (name params replaced) #:transparent)



(struct p1-adhoc (name predicate type) #:transparent)

(struct p1-overload (name context predicate body) #:transparent)

(struct p1-derive (name context predicate) #:transparent)



(struct p1-tag (name tag) #:transparent)



(struct p1-type-syn (name params type) #:transparent)

(struct p1-eff-syn (name params effects) #:transparent)

(struct p1-pred-syn (name params predicates) #:transparent)



(struct p1-check (name type) #:transparent)

(struct p1-function (name fixed-params body) #:transparent)

(struct p1-recursive (functions) #:transparent)



(struct p1-test (name compare left right))

(struct p1-law (name exhaustive compare left right))

(struct p1-compare (test-type comparison))



(struct p1-predicate (name qualified dotted))

(struct p1-type-app (left right))

(struct p1-tag-type-expr (flex rigid))

(struct p1-fixed-type-expr (flex rigid))

(struct p1-rigid-type-seq (elems))

(struct p1-flex-type-seq (elems dotted))

(struct p1-field-seq (fields row))

(struct p1-effect-seq (effects row))

(struct p1-effect (name params))

(struct p1-field (name type))

(struct p1-abelian-flex (name exponent))

(struct p1-abelian-rigid (alias name exponent))

(struct p1-type-flex (name))

(struct p1-type-rigid (alias name))



(struct p1-block (statements))

(struct p1-statement (patterns expr))

(struct p1-expr (words))



(struct p1-let (name body in))

(struct p1-rec (functions in))



(struct p1-handle (params body handlers return))

(struct p1-handler (op params body))



(struct p1-match (clauses otherwise))

(struct p1-match-rigid (patterns body))

(struct p1-match-flex (dotted patterns body))



(struct p1-if (condition then else))

(struct p1-while (condition body))

(struct p1-until (condition body))



(struct p1-for (kind clauses breaks body))

(struct p1-for-gen (name body))

(struct p1-for-filter (negate body))

(struct p1-for-length (size fill))

(struct p1-for-result (body))

(struct p1-for-break (final body))



(struct p1-anon-word (body))



