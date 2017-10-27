type token =
  | TOKEN
  | TYPE
  | LEFT
  | RIGHT
  | NONASSOC
  | START
  | PREC
  | PUBLIC
  | COLON
  | BAR
  | EOF
  | EQUAL
  | INLINE
  | LPAREN
  | RPAREN
  | COMMA
  | QUESTION
  | STAR
  | PLUS
  | PARAMETER
  | ON_ERROR_REDUCE
  | LID of (string Positions.located)
  | UID of (string Positions.located)
  | HEADER of (Stretch.t)
  | OCAMLTYPE of (Stretch.ocamltype)
  | PERCENTPERCENT of (Stretch.t Lazy.t)
  | ACTION of (Syntax.identifier option array -> Syntax.action)
  | ATTRIBUTE of (Syntax.attribute)
  | GRAMMARATTRIBUTE of (Syntax.attribute)
  | PERCENTATTRIBUTE

val grammar :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Syntax.partial_grammar
