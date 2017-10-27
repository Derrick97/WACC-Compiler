type token =
  | COLON
  | EOF
  | EOL
  | TERMINAL of (Grammar.Terminal.t * Lexing.position * Lexing.position)
  | NONTERMINAL of (Grammar.Nonterminal.t * Lexing.position * Lexing.position)
  | COMMENT of (string)

val optional_sentence :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> SentenceParserAux.sentence option
val entry :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> SentenceParserAux.located_sentence SentenceParserAux.or_comment list
