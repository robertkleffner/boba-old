#lang brag

program ::= constructors declarations main

constructors ::= data-constructor*

declarations ::= declaration*

@declaration
    ::= function
      | recursive

@main ::= /"main" /"=" simple-expr



data-constructor ::= constructor INTEGER



function ::= /"fun" identifier /"=" simple-expr

recursive ::= /"recursive" /"{" function+ /"}"



term-statement-block ::= /"{" term-statement* /"}"

term-statement ::= simple-expr ";"

simple-expr ::= word*

@word ::= term-statement-block
       | let-word
       | let-rec-word
       | handle-word
       | if-word
       | while-word
       | function-literal
       | list-literal
       | vector-literal | slice-literal
       | dictionary-literal
       | record-literal | extension | restriction | selection | update
       | variant-literal | embedding | case-word
       | bag-literal | bag-get | bag-put | bag-drop
       | union-literal | type-of
       | new-ref | get-ref | put-ref
       | identifier
       | predicate-name
       | constructor
       | operator-name
       | "do"
       | STRING
       | INTEGER
       | DECIMAL
       | CHAR



let-word ::= /"local" /"fun" term-variable /"=" simple-expr /"in" word

let-rec-word ::= /"local" /"recursive" (/"fun" term-variable /"=" simple-expr)+ /"in" word



handle-word ::= /"handle" term-variable* term-statement-block /"with" /"{" handler* return? /"}"

handler ::= operator-name term-variable* /"=>" simple-expr /";"

return ::= /"afterward" simple-expr /";"



if-word ::= /"if" term-statement-block term-statement-block term-statement-block

while-word ::= /"while" term-statement-block term-statement-block



function-literal ::= /"(" simple-expr /")"



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

bag-get ::= /"bag-get" /"<" INTEGER /">"

bag-put ::= /"bag-put" /"<" INTEGER /">"

bag-drop ::= /"bag-drop" /"<" INTEGER /">"



union-literal ::= /"U[" word /"]"

type-of ::= /"typeof" /"{" (INTEGER /"=>" simple-expr /";")+ /"otherwise" /"=>" simple-expr /";" /"}"



new-ref ::= /"new@"

get-ref ::= /"get@"

put-ref ::= /"put@"



property-name ::= PROPERTY_NAME
operator-name ::= OPERATOR_NAME
predicate-name ::= (term-variable "::")* PREDICATE_NAME
term-variable ::= SMALL_NAME
term-constructor ::= BIG_NAME
constructor ::= (term-variable "::")* term-constructor
identifier ::= (term-variable "::")* term-variable