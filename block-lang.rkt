#lang brag

program ::= data-constructor* block*

data-constructor ::= term-constructor INTEGER



block ::= /"top" func-variable /"=" word
        | /"def" func-variable /"with" func-variable* /"=" word
        | /"op" func-variable term-variable* /"=" word

statement-block ::= /"{" simple-expr /"}"

simple-expr ::= word*

@word ::= statement-block
       | assign-word
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
       | new-ref | get-ref | put-ref
       | term-variable
       | func-variable
       | predicate-name
       | term-constructor
       | operator-name
       | "do"
       | STRING
       | INTEGER
       | DECIMAL
       | CHAR



assign-word ::= /"assign" term-variable* /"in" word



let-word ::= /"local" /"fun" func-variable /"in" word

let-rec-word ::= /"local" /"recursive" func-variable+ /"in" word



handle-word ::= /"handle" handle-params statement-block /"with" /"{" operator-name* term-variable /"}"

handle-params ::= term-variable*



if-word ::= /"if" statement-block statement-block

while-word ::= /"while" statement-block statement-block



function-literal ::= /"(" func-variable /")"



list-literal ::= /"L[" /"]"

vector-literal ::= /"V[" /"]"

slice-literal ::= /"S[" slice-literal? word ".." word /"]"



dictionary-literal ::= /"D{" /"}"



record-literal ::= /"R{" /"}"

extension ::= /"+" term-variable

restriction ::= /"-" term-variable

selection ::= /"<-" term-variable

update ::= /"->" term-variable



variant-literal ::= /"V{" term-variable /"}"

embedding ::= /"|" term-variable

case-word ::= /"case" /"{" (term-variable /"=>" simple-expr /";")+ /"otherwise" /"=>" simple-expr /";" /"}"



new-ref ::= /"new@"

get-ref ::= /"get@"

put-ref ::= /"put@"



operator-name ::= OPERATOR_NAME
predicate-name ::= PREDICATE_NAME
term-variable ::= SMALL_NAME
func-variable ::= FUNC_NAME
term-constructor ::= BIG_NAME
