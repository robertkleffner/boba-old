#lang brag

unit ::= imports declarations (main | export)
imports ::= import*
declarations ::= declaration*

import ::= /"import" (STRING | remote) /"as" SMALL_NAME
         | /"import" name-list (STRING | remote) /"as" SMALL_NAME

remote ::= SMALL_NAME /"." SMALL_NAME /"." SMALL_NAME /":" INTEGER /"." INTEGER /"." INTEGER

@declaration
    ::= data
      | data-rec
      | pattern
      | ad-hoc
      | overload
      | deriving
      | tag
      | synonym
      | check
      | function
      | recursive
      | test
      | law

main ::= /"main" /"=" simple-expr

export ::= /"export" name-list

@name-list ::= /"{" (SMALL_NAME | BIG_NAME)* /"}"



data ::= /"data" type-constructor data-params (/"tagged" type-variable)? (/"=" data-constructor (/"|" data-constructor)*)?

data-params ::= type-variable*

data-constructor ::= term-constructor type-expression*

data-rec ::= /"recursive" /"{" data+ /"}"



pattern ::= /"pattern" term-constructor term-variable* /"=" pattern-expression



ad-hoc ::= /"adhoc" term-variable /"with" predicate-name term-variable /"=>" type-expression

overload ::= /"overload" single-predicate predicate-context? /"with" term-variable /"=" simple-expr

deriving ::= /"derive" /"overload" single-predicate predicate-context? /"with" term-variable

predicate-context ::= /"<=" simple-predicate (/"," simple-predicate)*



tag ::= /"tag" term-constructor /"with" type-constructor



synonym ::= /"synonym" type-constructor type-variable* /"=" type-expression
          | /"synonym" operator-name type-variable* /"=" effect (/"," effect)*
          | /"synonym" simple-predicate /"=" simple-predicate (/"," simple-predicate)*



check ::= /"check" term-variable /":" type-expression

function ::= /"fun" term-variable fixed-size-params? /"=" simple-expr

recursive ::= /"recursive" /"{" function+ /"}"



test ::= /"test" property-name /"=" simple-expr assert-type simple-expr

law ::= "exhaustive"? /"law" property-name term-variable* /"=" simple-expr assert-type simple-expr

assert-type ::= ("is-roughly" | "satisfies" | "violates")
              | ("is" | "is-not") term-statement-block?



predicate ::= predicate-name type-expression
            | /"(" predicate-name type-expression /")" "..."

single-predicate ::= predicate-name type-expression

simple-predicate ::= predicate-name type-variable

type-expression ::= "(" type-expression type-expression ")"
                  | /"<" (tag-type-expression | fixed-size-type-expression) /">"
                  | /"[" (type-sequence | field-sequence | effect-sequence) /"]"
                  | qualified-type-constructor
                  | type-constructor
                  | type-variable
                  | primitive-type

type-sequence ::= (type-expression "...")? type-expression*

qualified-type-constructor ::= term-variable /"::" type-constructor

primitive-type ::= "Bool"
                 | "Char"
                 | "I8" | "U8" | "I16" | "U16" | "I32" | "U32" | "I64" | "U64" | "F32" | "F64" | "ISize" | "USize"
                 | "-->"
                 | "Tuple" | "List" | "Vector" | "Slice" | "Dict" | "Record" | "Variant" | "Bag" | "Union"
                 | "Ref"

effect ::= operator-name type-expression*

effect-sequence ::= type-variable /"..." (effect (/"," effect)*)?

field ::= term-variable /":" type-expression

field-sequence ::= type-variable /"..." (field (/"," field)*)?

fixed-size-type-expression ::= INTEGER (/"+" fixed-size-variable (/"+" fixed-size-variable)*)?

fixed-size-variable ::= INTEGER? type-variable

tag-type-expression ::= tag-variable* tag-constructor*

tag-variable ::= type-variable (/"^" INTEGER)?

tag-constructor ::= type-constructor (/"^" INTEGER)?



term-statement-block ::= /"{" term-statement* /"}"

term-statement ::= /"let" pattern-expression* /"<=" simple-expr /";"
                 | simple-expr ";"

simple-expr ::= word*

@word ::= term-statement-block
       | let-word
       | let-rec-word
       | handle-word
       | match-word
       | if-word
       | while-word
       | for-comprehension
       | function-literal
       | tuple-literal
       | list-literal
       | vector-literal | slice-literal
       | dictionary-literal
       | record-literal | extension | restriction | selection | update
       | variant-literal | embedding | case-word
       | bag-literal | bag-get | bag-put | bag-drop
       | union-literal | type-of
       | run | new-ref | get-ref | put-ref
       | untag
       | identifier
       | constructor
       | operator-name
       | "do"
       | STRING
       | INTEGER
       | DECIMAL
       | CHAR
       | primitive-function



let-word ::= /"local" /"fun" term-variable /"=" simple-expr /"in" word

let-rec-word ::= /"local" /"recursive" (/"fun" term-variable /"=" simple-expr)+ /"in" word



handle-word ::= /"handle" term-variable* term-statement-block /"with" /"{" handler* return? /"}"

handler ::= operator-name term-variable* /"=>" simple-expr /";"

return ::= /"afterward" simple-expr /";"



match-word ::= /"match" match-clause+ /"otherwise" /"=>" simple-expr /";"
             | /"match" match-all-clause+ /"otherwise" /"=>" simple-expr /";"

match-clause ::= pattern-expression+ /"=>" simple-expr /";"

match-all-clause ::= pattern-expression "..." pattern-expression* /"=>" simple-expr /";"



if-word ::= /"if" term-statement-block term-statement-block term-statement-block

while-word ::= /"while" term-statement-block term-statement-block

until-word ::= /"until" term-statement-block term-statement-block



for-comprehension ::= FOR_KIND /"{" for-clause* /"}" break-clause* term-statement-block

for-clause ::= term-variable /"<=" simple-expr /";"
             | /"when" term-statement
             | /"unless" term-statement
             | break-clause
             | /"length" fixed-size-term-expression (/"fill" simple-expr)? /";"
             | /"result" simple-expr /";"

break-clause ::= /"break" simple-expr /";"
               | /"final" simple-expr /";"



function-literal ::= /"(" simple-expr /")"



tuple-literal ::= /"T[" simple-expr (word "...")? /"]"

list-literal ::= /"L[" simple-expr (word "..." simple-expr)? /"]"

vector-literal ::= /"V[" simple-expr (word "..." simple-expr)? /"]"

slice-literal ::= /"S[" slice-literal? word ".." word /"]"



dictionary-literal ::= /"D{" (word /"=" word)* (term-variable "...")? /"}"



record-literal ::= /"R{" (term-variable /"=" word)* (term-variable "...")? /"}"

extension ::= /"+" term-variable

restriction ::= /"-" term-variable

selection ::= /"<-" (term-variable /".")* term-variable

update ::= /"->" (term-variable /".")* term-variable



variant-literal ::= /"V{" term-variable /"=" word /"}"

embedding ::= /"|" term-variable

case-word ::= /"case" /"{" (term-variable /"=>" simple-expr /";")+ /"otherwise" /"=>" simple-expr /";" /"}"



bag-literal ::= /"B[" word* (word "...")? /"]"

bag-get ::= /"bag-get" /"<" type-expression /">"

bag-put ::= /"bag-put"

bag-drop ::= /"bag-drop" /"<" type-expression /">"



union-literal ::= /"U[" word /"]"

type-of ::= /"typeof" /"{" (type-expression /"=>" simple-expr /";")+ /"otherwise" /"=>" simple-expr /";" /"}"



run ::= /"with-state" term-statement-block

new-ref ::= /"new@"

get-ref ::= /"get@"

put-ref ::= /"put@"



untag ::= /"/" (term-variable /"::")? term-variable



pattern-expression
    ::= term-variable (/"is" pattern-expression)?
      | INTEGER
      | DECIMAL
      | STRING
      | CHAR
      | "True"
      | "False"
      | "@" pattern-expression
      | term-constructor
      | /"(" term-constructor pattern-expression* /")"
      | WILDCARD
      | tuple-pattern
      | list-pattern
      | vector-pattern
      | slice-pattern
      | record-pattern
      | dictionary-pattern

tuple-pattern ::= /"T[" pattern-expression* (pattern-expression "...")? /"]"

list-pattern ::= /"L[" pattern-expression* (pattern-expression "..." pattern-expression*)? /"]"

vector-pattern ::= /"V[" pattern-expression* (pattern-expression "..." pattern-expression*)? /"]"

slice-pattern ::= /"S[" pattern-expression* (pattern-expression "..." pattern-expression*)? /"]"

record-pattern ::= /"R{" (term-variable /"=" pattern-expression)* (term-variable "...")? /"}"

dictionary-pattern ::= /"D{" (pattern-expression /"=" pattern-expression)* (term-variable "...")? /"}"



primitive-function
    ::= "True"
      | "False"



fixed-size-params ::= /"<" term-variable+ /">"
fixed-size-term-expression ::= (fixed-size-term-factor /"+")* fixed-size-term-factor
fixed-size-term-factor ::= INTEGER | term-variable | INTEGER term-variable
property-name ::= PROPERTY_NAME
operator-name ::= OPERATOR_NAME
predicate-name ::= (term-variable /"::")? PREDICATE_NAME
type-variable ::= SMALL_NAME
term-variable ::= SMALL_NAME
type-constructor ::= BIG_NAME
term-constructor ::= BIG_NAME
constructor ::= (term-variable /"::")? term-constructor
identifier ::= (term-variable /"::")? term-variable (/"<" fixed-size-term-expression /">")?