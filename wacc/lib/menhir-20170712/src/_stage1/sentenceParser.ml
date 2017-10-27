type token =
  | COLON
  | EOF
  | EOL
  | TERMINAL of (Grammar.Terminal.t * Lexing.position * Lexing.position)
  | NONTERMINAL of (Grammar.Nonterminal.t * Lexing.position * Lexing.position)
  | COMMENT of (string)

open Parsing;;
let _ = parse_error;;
# 38 "sentenceParser.mly"

  open SentenceParserAux

  (* Removing the position information in a terminal or non-terminal symbol. *)

  let strip_symbol (x, _, _) = x

  (* Removing the position information in a sentence. *)

  let strip_sentence (nto, terminals) =
    Option.map strip_symbol nto,
    List.map strip_symbol terminals

  (* Computing the start and end positions of a sentence. *)

  let locate_sentence (nto, terminals) =
    let opening =
      match nto, terminals with
      | Some (_, opening, _), _
      | None, (_, opening, _) :: _ ->
          opening
      | None, [] ->
          Lexing.dummy_pos (* cannot happen *)
    and closing =
      match nto, List.rev terminals with
      | _, (_, _, closing) :: _
      | Some (_, _, closing), _ ->
          closing
      | None, [] ->
          Lexing.dummy_pos (* cannot happen *)
    in
    Positions.two opening closing,
    strip_sentence (nto, terminals)

# 47 "sentenceParser.ml"
let yytransl_const = [|
  257 (* COLON *);
    0 (* EOF *);
  258 (* EOL *);
    0|]

let yytransl_block = [|
  259 (* TERMINAL *);
  260 (* NONTERMINAL *);
  261 (* COMMENT *);
    0|]

let yylhs = "\255\255\
\003\000\004\000\004\000\004\000\001\000\002\000\002\000\005\000\
\005\000\006\000\006\000\000\000\000\000"

let yylen = "\002\000\
\002\000\000\000\002\000\002\000\001\000\001\000\001\000\004\000\
\002\000\000\000\002\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\006\000\000\000\000\000\012\000\007\000\
\000\000\000\000\000\000\013\000\000\000\005\000\011\000\000\000\
\009\000\004\000\003\000\001\000\000\000\008\000"

let yydgoto = "\003\000\
\011\000\007\000\012\000\013\000\014\000\009\000"

let yysindex = "\007\000\
\001\000\002\255\000\000\000\000\007\255\011\255\000\000\000\000\
\012\255\002\255\002\255\000\000\013\000\000\000\000\000\007\255\
\000\000\000\000\000\000\000\000\013\255\000\000"

let yyrindex = "\000\000\
\014\255\004\000\000\000\000\000\014\255\000\000\000\000\000\000\
\000\000\004\000\004\000\000\000\000\000\000\000\000\000\014\255\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\248\255\016\000\251\255"

let yytablesize = 262
let yytable = "\015\000\
\004\000\018\000\019\000\002\000\005\000\006\000\010\000\001\000\
\002\000\005\000\021\000\016\000\020\000\017\000\022\000\010\000\
\008\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\005\000\006\000\010\000"

let yycheck = "\005\000\
\000\000\010\000\011\000\000\000\003\001\004\001\005\001\001\000\
\002\000\003\001\016\000\001\001\000\000\002\001\002\001\002\001\
\001\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\004\001\002\001"

let yynames_const = "\
  COLON\000\
  EOF\000\
  EOL\000\
  "

let yynames_block = "\
  TERMINAL\000\
  NONTERMINAL\000\
  COMMENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'located_sentences_or_comments) in
    Obj.repr(
# 88 "sentenceParser.mly"
  ( _1 )
# 179 "sentenceParser.ml"
               : SentenceParserAux.located_sentence SentenceParserAux.or_comment list))
; (fun __caml_parser_env ->
    Obj.repr(
# 92 "sentenceParser.mly"
  ( [] )
# 185 "sentenceParser.ml"
               : 'located_sentences_or_comments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : located_sentence) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'located_sentences_or_comments) in
    Obj.repr(
# 93 "sentenceParser.mly"
                                                 ( Thing   _1 :: _2 )
# 193 "sentenceParser.ml"
               : 'located_sentences_or_comments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'located_sentences_or_comments) in
    Obj.repr(
# 94 "sentenceParser.mly"
                                                 ( Comment _1 :: _2 )
# 201 "sentenceParser.ml"
               : 'located_sentences_or_comments))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sentence) in
    Obj.repr(
# 98 "sentenceParser.mly"
  ( locate_sentence _1 )
# 208 "sentenceParser.ml"
               : located_sentence))
; (fun __caml_parser_env ->
    Obj.repr(
# 103 "sentenceParser.mly"
    ( None )
# 214 "sentenceParser.ml"
               : SentenceParserAux.sentence option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'sentence) in
    Obj.repr(
# 105 "sentenceParser.mly"
    ( Some (strip_sentence _1) )
# 221 "sentenceParser.ml"
               : SentenceParserAux.sentence option))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : Grammar.Nonterminal.t * Lexing.position * Lexing.position) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'terminals) in
    Obj.repr(
# 111 "sentenceParser.mly"
    ( Some _1, _3 )
# 229 "sentenceParser.ml"
               : 'sentence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'terminals) in
    Obj.repr(
# 113 "sentenceParser.mly"
    ( None, _1 )
# 236 "sentenceParser.ml"
               : 'sentence))
; (fun __caml_parser_env ->
    Obj.repr(
# 118 "sentenceParser.mly"
    ( [] )
# 242 "sentenceParser.ml"
               : 'terminals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Grammar.Terminal.t * Lexing.position * Lexing.position) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'terminals) in
    Obj.repr(
# 120 "sentenceParser.mly"
    ( _1 :: _2 )
# 250 "sentenceParser.ml"
               : 'terminals))
(* Entry optional_sentence *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
(* Entry entry *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let optional_sentence (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : SentenceParserAux.sentence option)
let entry (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 2 lexfun lexbuf : SentenceParserAux.located_sentence SentenceParserAux.or_comment list)
