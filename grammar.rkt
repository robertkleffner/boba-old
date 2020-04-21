#lang brag

unit ::= import* declaration* (main | export)

import ::= /"import" (STRING | REMOTE) /"as" SMALL_NAME
         | /"import" name-list (STRING | REMOTE) /"as" SMALL_NAME

declaration
    ::= data
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

name-list ::= /"{" (SMALL_NAME | BIG_NAME)* /"}"



data ::= (/"modify" type-variable)? /"noun" type-constructor type-variable* (/"=" data-constructor (/"|" data-constructor)*)?

data-constructor ::= term-constructor type-expression*



pattern ::= /"pronoun" term-constructor term-variable* /"=" pattern-expression



ad-hoc ::= /"adverb" term-variable /"with" simple-predicate /"=>" function-type

overload ::= /"modify" single-predicate predicate-context? /"with" term-variable /"=" simple-expr

deriving ::= /"derive" /"modify" single-predicate predicate-context? /"with" term-variable

predicate-context ::= /"<=" predicate (/"," predicate)*



tag ::= /"adjective" term-constructor /"with" type-constructor



synonym ::= /"synonym" type-constructor type-variable* /"=" type-expression
          | /"synonym" operator-name type-variable* /"=" effect (/"," effect)*
          | /"synonym" simple-predicate /"=" simple-predicate (/"," simple-predicate)*



check ::= /"check" term-variable /":" type-expression

function ::= /"verb" term-variable fixed-size-params? /"=" term-statement-block

recursive ::= /"recursive" /"{" function+ /"}"



test ::= /"test" property-name /"=" simple-expr (/"is-roughly" | /"satisfies" | /"violates" | /"is" | /"is-not") term-statement-block? simple-expr

law ::= "exhaustive"? /"law" property-name term-variable* /"=" simple-expr (/"is-roughly" | /"satisfies" | /"violates" | /"is" | /"is-not") term-statement-block? simple-expr



predicate ::= predicate-name type-expression
            | /"(" predicate-name type-expression /")" /"..."

single-predicate ::= predicate-name type-expression

simple-predicate ::= predicate-name type-variable

type-expression
    ::= function-type
      | tuple-type
      | list-type
      | vector-type
      | slice-type
      | dictionary-type
      | record-type
      | variant-type
      | bag-type
      | union-type
      | reference-type
      | type-application

type-sequence ::= (type-expression "...")? type-expression*

function-type ::= /"(" type-sequence /"-->" type-sequence /")"
                | /"(" type-sequence /"-[" effect (/"," effect)* /"]->" type-sequence /")"

tuple-type ::= /"T[" type-sequence /"]"

list-type ::= /"L[" type-expression /"]"

vector-type ::= /"V[" type-expression /"]" /"<" fixed-size-type-expression /">"

slice-type ::= /"S[" type-expression /"]" /"<" fixed-size-type-expression /">"

dictionary-type ::= /"D{" type-expression /"," type-expression /"}"

record-type ::= /"R{" (field (/"," field)*)? /"}"

variant-type ::= /"V{" (field (/"," field)*)? /"}"

bag-type ::= /"B[" type-expression* /"]"

union-type ::= /"U[" type-expression* /"]"

reference-type ::= /"@" type-expression

type-application ::= "(" type-application type-expression ")"
                   | type-application /"<" tag-type-expression /">"
                   | qualified-type-constructor
                   | type-constructor
                   | type-variable
                   | primitive-type

qualified-type-constructor ::= term-variable /"::" type-constructor

primitive-type ::= "Bool"
                 | "Char"
                 | "I8" | "U8" | "I16" | "U16" | "I32" | "U32" | "I64" | "U64" | "F32" | "F64" | "ISize" | "USize"

effect ::= operator-name type-expression*

field ::= term-variable /":" type-expression

fixed-size-type-expression ::= fixed-size-variable (/"+" fixed-size-variable)*
                             | INTEGER (/"+" fixed-size-variable (/"+" fixed-size-variable)*)?

fixed-size-variable ::= INTEGER? type-variable

tag-type-expression ::= tag-variable* tag-constructor*

tag-variable ::= type-variable (/"^" INTEGER)?

tag-constructor ::= type-constructor (/"^" INTEGER)?



term-statement-block ::= /"{" term-statement* /"}"

term-statement ::= /"var" pattern-expression* /"<=" simple-expr /";"
                 | simple-expr ";"

simple-expr ::= word*

word ::= term-statement-block
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



let-word ::= /"local" /"word" term-variable /"=" simple-expr /"in" word

let-rec-word ::= /"local" /"recursive" (/"word" term-variable /"=" simple-expr)+ /"in" word



handle-word ::= /"handle" term-variable term-statement-block /"with" /"{" handler* return? /"}"

handler ::= operator-name term-variable* /"=>" word /";"

return ::= /"afterward" simple-expr /";"



match-word ::= /"match" match-clause+ /"otherwise" /"=>" simple-expr /";"
             | /"match" match-all-clause+ /"otherwise" /"=>" simple-expr /";"

match-clause ::= pattern-expression+ /"=>" simple-expr /";"

match-all-clause ::= pattern-expression /"..." pattern-expression* /"=>" simple-expr /";"



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



tuple-literal ::= /"T[" simple-expr (word /"...")? /"]"

list-literal ::= /"L[" simple-expr (word /"..." simple-expr)? /"]"

vector-literal ::= /"V[" simple-expr (word /"..." simple-expr)? /"]"

slice-literal ::= /"S[" slice-literal? word /".." word /"]"



dictionary-literal ::= /"D{" (word /"=" word)* (term-variable /"...")? /"}"



record-literal ::= /"R{" (term-variable /"=" word)* (term-variable /"...")? /"}"

extension ::= /"+" term-variable

restriction ::= /"-" term-variable

selection ::= /"->" (term-variable /".")* term-variable

update ::= /"<-" (term-variable /".")* term-variable



variant-literal ::= /"V{" term-variable /"=" word /"}"

embedding ::= /"|" term-variable

case-word ::= /"case" /"{" (term-variable /"=>" simple-expr /";")+ /"otherwise" /"=>" simple-expr /";" /"}"



bag-literal ::= /"B[" word* (word /"...")? /"]"

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

tuple-pattern ::= /"T[" pattern-expression* (pattern-expression /"...")? /"]"

list-pattern ::= /"L[" pattern-expression* (pattern-expression /"..." pattern-expression*)? /"]"

vector-pattern ::= /"V[" pattern-expression* (pattern-expression /"..." pattern-expression*)? /"]"

slice-pattern ::= /"S[" pattern-expression* (pattern-expression /"..." pattern-expression*)? /"]"

record-pattern ::= /"R{" (term-variable /"=" pattern-expression)* (term-variable /"...")? /"}"

dictionary-pattern ::= /"D{" (term-variable /"=" pattern-expression)* (term-variable /"...")? /"}"



primitive-function
    ::= "True"
      | "False"



fixed-size-params ::= /"<" term-variable+ /">"
fixed-size-term-expression ::= (fixed-size-term-factor /"+")* fixed-size-term-factor
fixed-size-term-factor ::= INTEGER | term-variable | INTEGER term-variable
property-name ::= PROPERTY_NAME
operator-name ::= OPERATOR_NAME
predicate-name ::= PREDICATE_NAME
type-variable ::= SMALL_NAME
term-variable ::= SMALL_NAME
type-constructor ::= BIG_NAME
term-constructor ::= BIG_NAME
constructor ::= (term-variable /"::")? term-constructor
identifier ::= (term-variable /"::")? term-variable (/"<" fixed-size-term-expression /">")?