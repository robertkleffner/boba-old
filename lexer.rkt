#lang racket/base

(require brag/support)
(require br-parser-tools/lex)

(define-lex-abbrev digits (:+ (char-set "0123456789")))

(define (tokenize ip)
  (port-count-lines! ip)
  (define boba-lexer
    (lexer-src-pos
     ["import" (token "import" lexeme)]
     ["as" (token "as" lexeme)]
     ["main" (token "main" lexeme)]
     ["export" (token "export" lexeme)]
     ["tagged" (token "tagged" lexeme)]
     ["data" (token "data" lexeme)]
     ["pattern" (token "pattern" lexeme)]
     ["tag" (token "tag" lexeme)]
     ["adhoc" (token "adhoc" lexeme)]
     ["with" (token "with" lexeme)]
     ["derive" (token "derive" lexeme)]
     ["overload" (token "overload" lexeme)]
     ["synonym" (token "synonym" lexeme)]
     ["fun" (token "fun" lexeme)]
     ["recursive" (token "recursive" lexeme)]
     ["test" (token "test" lexeme)]
     ["law" (token "law" lexeme)]
     ["exhaustive" (token "exhaustive" lexeme)]
     ["is-roughly" (token "is-roughly" lexeme)]
     ["satisfies" (token "satisfies" lexeme)]
     ["violates" (token "violates" lexeme)]
     ["is" (token "is" lexeme)]
     ["is-not" (token "is-not" lexeme)]
     ["check" (token "check" lexeme)]
     ["let" (token "let" lexeme)]
     ["local" (token "local" lexeme)]
     ["in" (token "in" lexeme)]
     ["handle" (token "handle" lexeme)]
     ["afterward" (token "afterward" lexeme)]
     ["match" (token "match" lexeme)]
     ["otherwise" (token "otherwise" lexeme)]
     ["if" (token "if" lexeme)]
     ["while" (token "while" lexeme)]
     ["until" (token "until" lexeme)]
     ["case" (token "case" lexeme)]
     ["bag-get" (token "bag-get" lexeme)]
     ["bag-put" (token "bag-put" lexeme)]
     ["bag-drop" (token "bag-drop" lexeme)]
     ["typeof" (token "typeof" lexeme)]
     ["with-state" (token "with-state" lexeme)]
     ["new@" (token "new@" lexeme)]
     ["get@" (token "get@" lexeme)]
     ["put@" (token "put@" lexeme)]
     ["do" (token "do" lexeme)]

     [(:seq (:or "for/" "for*/")
            (:or "list" "lists" "vector" "tuple" "dict" "and" "or" "sum" "product" "first" "last" "foldl" "foldr"))
      (token 'FOR_KIND lexeme)]
     ["when" (token "when" lexeme)]
     ["unless" (token "unless" lexeme)]
     ["length" (token "length" lexeme)]
     ["fill" (token "fill" lexeme)]
     ["result" (token "result" lexeme)]
     ["break" (token "break" lexeme)]
     ["final" (token "final" lexeme)]

     [(:or "Bool" "Char" "I8" "U8" "I16" "U16" "I32" "U32" "I64" "U64" "ISize" "USize" "F32" "F64")
      (token lexeme lexeme)]
     [(:or "True" "False")
      (token lexeme lexeme)]
     
     ["(" (token "(" lexeme)]
     [")" (token ")" lexeme)]
     ["-->" (token "-->" lexeme)]
     ["-[" (token "-[" lexeme)]
     ["]->" (token "]->" lexeme)]
     ["T[" (token "T[" lexeme)]
     ["L[" (token "L[" lexeme)]
     ["V[" (token "V[" lexeme)]
     ["S[" (token "S[" lexeme)]
     ["B[" (token "B[" lexeme)]
     ["U[" (token "U[" lexeme)]
     ["D{" (token "D{" lexeme)]
     ["R{" (token "R{" lexeme)]
     ["V{" (token "V{" lexeme)]
     
     ["..." (token "..." lexeme)]
     ["=>" (token "=>" lexeme)]
     ["<=" (token "<=" lexeme)]
     ["->" (token "->" lexeme)]
     ["<-" (token "<-" lexeme)]
     ["::" (token "::" lexeme)]
     ["+" (token "+" lexeme)]
     ["-" (token "-" lexeme)]
     ["^" (token "^" lexeme)]
     ["/" (token "/" lexeme)]
     [":" (token ":" lexeme)]
     [">" (token ">" lexeme)]
     ["<" (token "<" lexeme)]
     ["|" (token "|" lexeme)]
     ["=" (token "=" lexeme)]
     [";" (token ";" lexeme)]
     ["{" (token "{" lexeme)]
     ["}" (token "}" lexeme)]
     ["]" (token "]" lexeme)]
     ["@" (token "@" lexeme)]
     ["." (token "." lexeme)]
     ["_" (token 'WILDCARD lexeme)]

     [(from/to "\"" "\"")
      (token 'STRING
             (substring lexeme
                        1 (sub1 (string-length lexeme))))]
     [(from/to "\'" "\'")
      (token 'CHAR
             (substring lexeme
                        1 (sub1 (string-length lexeme))))]
     [(:seq (:? "-") digits)
      (token 'INTEGER (string->number lexeme))]
     [(:or (:seq (:? "-") (:? digits) "." digits)
           (:seq (:? "-") digits "."))
      (token 'DECIMAL (string->number lexeme))]
     
     [(:seq lower-case (:* (:or alphabetic numeric "-")))
      (token 'SMALL_NAME lexeme)]
     [(:seq upper-case (:* (:or alphabetic numeric "-")))
      (token 'BIG_NAME lexeme)]
     [(:seq upper-case (:* (:or alphabetic numeric "-")) "?")
      (token 'PREDICATE_NAME lexeme)]
     [(:seq lower-case (:* (:or alphabetic numeric "-")) "!")
      (token 'OPERATOR_NAME lexeme)]
     [(:seq "#" lower-case (:* (:or alphabetic numeric "-")))
      (token 'PROPERTY_NAME lexeme)]
     
     [whitespace
      (token 'WHITESPACE lexeme #:skip? #t)]
     [(eof)
      (void)]))
  (define (next-token) (boba-lexer ip))
  next-token)

(provide tokenize)