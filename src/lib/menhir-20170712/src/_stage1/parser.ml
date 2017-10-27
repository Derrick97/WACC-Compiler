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

open Parsing;;
let _ = parse_error;;
# 23 "parser.mly"

open Syntax
open Positions

# 41 "parser.ml"
let yytransl_const = [|
  257 (* TOKEN *);
  258 (* TYPE *);
  259 (* LEFT *);
  260 (* RIGHT *);
  261 (* NONASSOC *);
  262 (* START *);
  263 (* PREC *);
  264 (* PUBLIC *);
  265 (* COLON *);
  266 (* BAR *);
    0 (* EOF *);
  267 (* EQUAL *);
  268 (* INLINE *);
  269 (* LPAREN *);
  270 (* RPAREN *);
  271 (* COMMA *);
  272 (* QUESTION *);
  273 (* STAR *);
  274 (* PLUS *);
  275 (* PARAMETER *);
  276 (* ON_ERROR_REDUCE *);
  285 (* PERCENTATTRIBUTE *);
    0|]

let yytransl_block = [|
  277 (* LID *);
  278 (* UID *);
  279 (* HEADER *);
  280 (* OCAMLTYPE *);
  281 (* PERCENTPERCENT *);
  282 (* ACTION *);
  283 (* ATTRIBUTE *);
  284 (* GRAMMARATTRIBUTE *);
    0|]

let yylhs = "\255\255\
\001\000\006\000\006\000\004\000\004\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\008\000\
\008\000\012\000\012\000\012\000\013\000\013\000\016\000\016\000\
\015\000\015\000\014\000\014\000\009\000\009\000\010\000\010\000\
\005\000\005\000\017\000\018\000\018\000\018\000\018\000\018\000\
\019\000\019\000\023\000\023\000\024\000\024\000\025\000\025\000\
\026\000\026\000\011\000\011\000\020\000\020\000\027\000\027\000\
\027\000\022\000\022\000\021\000\029\000\029\000\028\000\028\000\
\030\000\030\000\003\000\031\000\031\000\002\000\002\000\000\000"

let yylen = "\002\000\
\004\000\001\000\001\000\000\000\002\000\001\000\003\000\002\000\
\003\000\003\000\002\000\002\000\001\000\003\000\002\000\000\000\
\001\000\001\000\001\000\001\000\000\000\003\000\001\000\001\000\
\000\000\001\000\000\000\002\000\000\000\004\000\000\000\002\000\
\000\000\002\000\008\000\000\000\001\000\001\000\002\000\002\000\
\000\000\003\000\001\000\003\000\000\000\003\000\001\000\003\000\
\002\000\002\000\000\000\003\000\000\000\001\000\001\000\001\000\
\001\000\000\000\003\000\003\000\000\000\002\000\001\000\002\000\
\002\000\003\000\002\000\000\000\002\000\002\000\004\000\002\000"

let yydefred = "\000\000\
\004\000\000\000\072\000\000\000\000\000\000\000\018\000\019\000\
\020\000\000\000\000\000\051\000\006\000\033\000\013\000\051\000\
\005\000\021\000\017\000\029\000\051\000\031\000\000\000\012\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\032\000\
\026\000\000\000\000\000\002\000\000\000\003\000\001\000\034\000\
\000\000\000\000\014\000\000\000\000\000\023\000\024\000\000\000\
\000\000\039\000\040\000\000\000\028\000\022\000\000\000\000\000\
\049\000\055\000\057\000\056\000\050\000\000\000\030\000\000\000\
\000\000\000\000\000\000\046\000\000\000\000\000\000\000\000\000\
\048\000\000\000\042\000\054\000\068\000\044\000\000\000\058\000\
\000\000\000\000\068\000\064\000\000\000\000\000\000\000\000\000\
\069\000\000\000\067\000\000\000\068\000\060\000\062\000\000\000\
\070\000\066\000\059\000\000\000\071\000"

let yydgoto = "\002\000\
\003\000\089\000\079\000\004\000\026\000\039\000\017\000\020\000\
\029\000\023\000\025\000\018\000\028\000\043\000\034\000\048\000\
\040\000\041\000\067\000\077\000\080\000\085\000\071\000\057\000\
\064\000\065\000\061\000\081\000\091\000\084\000\082\000"

let yysindex = "\008\000\
\000\000\000\000\000\000\225\255\007\255\023\255\000\000\000\000\
\000\000\035\255\064\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\020\255\000\000\
\060\255\004\000\255\254\060\255\060\255\060\255\020\255\000\000\
\000\000\251\254\082\255\000\000\090\255\000\000\000\000\000\000\
\251\254\089\255\000\000\251\254\085\255\000\000\000\000\112\255\
\104\255\000\000\000\000\089\255\000\000\000\000\089\255\251\254\
\000\000\000\000\000\000\000\000\000\000\113\255\000\000\103\255\
\170\255\251\254\118\255\000\000\251\254\120\255\119\255\139\255\
\000\000\251\254\000\000\000\000\000\000\000\000\152\255\000\000\
\122\255\042\255\000\000\000\000\154\255\158\255\251\254\162\255\
\000\000\129\255\000\000\152\255\000\000\000\000\000\000\251\254\
\000\000\000\000\000\000\129\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\017\255\000\000\000\000\000\000\
\000\000\031\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\071\000\000\000\
\138\255\013\255\149\255\178\255\042\000\189\255\078\000\000\000\
\000\000\000\000\022\255\000\000\070\255\000\000\000\000\000\000\
\000\000\080\255\000\000\000\000\000\000\000\000\000\000\051\255\
\109\255\000\000\000\000\250\254\000\000\000\000\218\255\000\000\
\000\000\000\000\000\000\000\000\000\000\166\255\000\000\000\000\
\175\255\000\000\000\000\000\000\000\000\182\255\000\000\097\255\
\000\000\000\000\000\000\000\000\000\000\000\000\150\255\000\000\
\000\000\001\255\000\000\000\000\006\000\001\000\000\000\095\000\
\000\000\003\255\000\000\176\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\003\255\000\000"

let yygindex = "\000\000\
\000\000\000\000\121\000\000\000\000\000\000\000\000\000\000\000\
\000\000\183\000\012\000\000\000\000\000\216\255\068\000\220\255\
\000\000\000\000\000\000\000\000\120\000\000\000\141\000\000\000\
\147\000\222\255\000\000\000\000\139\000\140\000\000\000"

let yytablesize = 378
let yytable = "\049\000\
\061\000\053\000\027\000\036\000\052\000\035\000\027\000\054\000\
\001\000\027\000\061\000\062\000\027\000\033\000\063\000\046\000\
\047\000\016\000\016\000\016\000\016\000\016\000\016\000\027\000\
\027\000\042\000\061\000\027\000\027\000\070\000\019\000\016\000\
\030\000\036\000\036\000\016\000\016\000\070\000\016\000\016\000\
\032\000\016\000\037\000\037\000\016\000\016\000\021\000\090\000\
\087\000\097\000\095\000\045\000\045\000\045\000\045\000\045\000\
\045\000\045\000\022\000\101\000\045\000\100\000\088\000\047\000\
\045\000\045\000\045\000\045\000\045\000\045\000\045\000\045\000\
\045\000\045\000\033\000\045\000\045\000\045\000\045\000\045\000\
\027\000\027\000\027\000\027\000\027\000\027\000\027\000\024\000\
\027\000\027\000\038\000\038\000\027\000\050\000\027\000\044\000\
\045\000\051\000\027\000\027\000\027\000\027\000\027\000\053\000\
\027\000\027\000\055\000\027\000\027\000\052\000\052\000\052\000\
\052\000\052\000\052\000\042\000\068\000\053\000\053\000\058\000\
\059\000\060\000\053\000\052\000\056\000\066\000\072\000\052\000\
\052\000\052\000\052\000\052\000\075\000\052\000\074\000\052\000\
\052\000\052\000\015\000\015\000\015\000\015\000\015\000\015\000\
\058\000\059\000\060\000\086\000\076\000\027\000\027\000\027\000\
\027\000\027\000\027\000\042\000\015\000\015\000\025\000\025\000\
\015\000\083\000\015\000\093\000\087\000\015\000\015\000\027\000\
\027\000\025\000\025\000\027\000\096\000\027\000\041\000\063\000\
\027\000\027\000\011\000\011\000\011\000\011\000\011\000\011\000\
\069\000\058\000\059\000\060\000\047\000\009\000\009\000\009\000\
\009\000\009\000\009\000\043\000\011\000\011\000\025\000\025\000\
\011\000\065\000\011\000\092\000\031\000\011\000\011\000\009\000\
\009\000\025\000\025\000\009\000\099\000\009\000\078\000\073\000\
\009\000\009\000\027\000\027\000\027\000\027\000\027\000\027\000\
\094\000\005\000\006\000\007\000\008\000\009\000\010\000\098\000\
\027\000\000\000\000\000\000\000\027\000\027\000\000\000\027\000\
\027\000\000\000\027\000\011\000\012\000\027\000\027\000\013\000\
\000\000\014\000\000\000\000\000\015\000\016\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\061\000\000\000\061\000\035\000\061\000\035\000\000\000\037\000\
\000\000\035\000\000\000\000\000\000\000\061\000\061\000\000\000\
\000\000\061\000\035\000\035\000\038\000\000\000\035\000\031\000\
\031\000\031\000\031\000\031\000\031\000\000\000\000\000\000\000\
\000\000\000\000\007\000\007\000\007\000\007\000\007\000\007\000\
\000\000\031\000\031\000\031\000\000\000\031\000\000\000\031\000\
\000\000\000\000\031\000\031\000\007\000\007\000\000\000\025\000\
\007\000\000\000\007\000\000\000\000\000\007\000\007\000\008\000\
\008\000\008\000\008\000\008\000\008\000\000\000\010\000\010\000\
\010\000\010\000\010\000\010\000\000\000\000\000\000\000\000\000\
\000\000\008\000\008\000\000\000\000\000\008\000\000\000\008\000\
\010\000\010\000\008\000\008\000\010\000\023\000\010\000\000\000\
\023\000\010\000\010\000\023\000\000\000\000\000\023\000\023\000\
\023\000\000\000\000\000\023\000\023\000\000\000\000\000\000\000\
\023\000\023\000"

let yycheck = "\034\000\
\000\000\042\000\009\001\000\000\041\000\000\000\013\001\044\000\
\001\000\007\001\010\001\052\000\010\001\015\001\055\000\021\001\
\022\001\001\001\002\001\003\001\004\001\005\001\006\001\021\001\
\022\001\027\001\026\001\016\000\026\001\066\000\024\001\015\001\
\021\000\021\001\022\001\019\001\020\001\074\000\022\001\023\001\
\021\001\025\001\021\001\022\001\028\001\029\001\024\001\082\000\
\007\001\090\000\087\000\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\024\001\100\000\010\001\096\000\021\001\022\001\
\014\001\015\001\016\001\017\001\018\001\019\001\020\001\021\001\
\022\001\023\001\015\001\025\001\026\001\027\001\028\001\029\001\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\024\001\
\009\001\010\001\021\001\022\001\013\001\012\001\015\001\028\000\
\029\000\008\001\019\001\020\001\021\001\022\001\023\001\007\001\
\025\001\026\001\022\001\028\001\029\001\001\001\002\001\003\001\
\004\001\005\001\006\001\027\001\014\001\021\001\022\001\016\001\
\017\001\018\001\026\001\015\001\013\001\013\001\009\001\019\001\
\020\001\021\001\022\001\023\001\014\001\025\001\015\001\027\001\
\028\001\029\001\001\001\002\001\003\001\004\001\005\001\006\001\
\016\001\017\001\018\001\026\001\010\001\001\001\002\001\003\001\
\004\001\005\001\006\001\027\001\019\001\020\001\021\001\022\001\
\023\001\010\001\025\001\010\001\007\001\028\001\029\001\019\001\
\020\001\021\001\022\001\023\001\011\001\025\001\009\001\026\001\
\028\001\029\001\001\001\002\001\003\001\004\001\005\001\006\001\
\015\001\016\001\017\001\018\001\014\001\001\001\002\001\003\001\
\004\001\005\001\006\001\014\001\019\001\020\001\021\001\022\001\
\023\001\026\001\025\001\083\000\022\000\028\001\029\001\019\001\
\020\001\021\001\022\001\023\001\093\000\025\001\074\000\069\000\
\028\001\029\001\001\001\002\001\003\001\004\001\005\001\006\001\
\086\000\001\001\002\001\003\001\004\001\005\001\006\001\092\000\
\015\001\255\255\255\255\255\255\019\001\020\001\255\255\022\001\
\023\001\255\255\025\001\019\001\020\001\028\001\029\001\023\001\
\255\255\025\001\255\255\255\255\028\001\029\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\008\001\255\255\010\001\008\001\012\001\008\001\255\255\012\001\
\255\255\012\001\255\255\255\255\255\255\021\001\022\001\255\255\
\255\255\025\001\021\001\022\001\025\001\255\255\025\001\001\001\
\002\001\003\001\004\001\005\001\006\001\255\255\255\255\255\255\
\255\255\255\255\001\001\002\001\003\001\004\001\005\001\006\001\
\255\255\019\001\020\001\021\001\255\255\023\001\255\255\025\001\
\255\255\255\255\028\001\029\001\019\001\020\001\255\255\022\001\
\023\001\255\255\025\001\255\255\255\255\028\001\029\001\001\001\
\002\001\003\001\004\001\005\001\006\001\255\255\001\001\002\001\
\003\001\004\001\005\001\006\001\255\255\255\255\255\255\255\255\
\255\255\019\001\020\001\255\255\255\255\023\001\255\255\025\001\
\019\001\020\001\028\001\029\001\023\001\007\001\025\001\255\255\
\010\001\028\001\029\001\013\001\255\255\255\255\016\001\017\001\
\018\001\255\255\255\255\021\001\022\001\255\255\255\255\255\255\
\026\001\027\001"

let yynames_const = "\
  TOKEN\000\
  TYPE\000\
  LEFT\000\
  RIGHT\000\
  NONASSOC\000\
  START\000\
  PREC\000\
  PUBLIC\000\
  COLON\000\
  BAR\000\
  EOF\000\
  EQUAL\000\
  INLINE\000\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  QUESTION\000\
  STAR\000\
  PLUS\000\
  PARAMETER\000\
  ON_ERROR_REDUCE\000\
  PERCENTATTRIBUTE\000\
  "

let yynames_block = "\
  LID\000\
  UID\000\
  HEADER\000\
  OCAMLTYPE\000\
  PERCENTPERCENT\000\
  ACTION\000\
  ATTRIBUTE\000\
  GRAMMARATTRIBUTE\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'declarations) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Stretch.t Lazy.t) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'rules) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'postlude) in
    Obj.repr(
# 61 "parser.mly"
    (
      {
        pg_filename          = ""; (* filled in by the caller *)
        pg_declarations      = List.rev _1;
        pg_rules             = _3;
        pg_postlude          = _4
      }
    )
# 311 "parser.ml"
               : Syntax.partial_grammar))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
    ( None )
# 317 "parser.ml"
               : 'postlude))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Stretch.t Lazy.t) in
    Obj.repr(
# 74 "parser.mly"
    ( Some (Lazy.force _1) )
# 324 "parser.ml"
               : 'postlude))
; (fun __caml_parser_env ->
    Obj.repr(
# 82 "parser.mly"
    ( [] )
# 330 "parser.ml"
               : 'declarations))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'declarations) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'declaration) in
    Obj.repr(
# 84 "parser.mly"
    ( _2 @ _1 )
# 338 "parser.ml"
               : 'declarations))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Stretch.t) in
    Obj.repr(
# 88 "parser.mly"
    ( [ unknown_pos (DCode _1) ] )
# 345 "parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'optional_ocamltype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'terminals) in
    Obj.repr(
# 91 "parser.mly"
    ( List.map (Positions.map (fun (terminal, attrs) -> DToken (_2, terminal, attrs))) _3 )
# 353 "parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'nonterminals) in
    Obj.repr(
# 94 "parser.mly"
    ( List.map (Positions.map (fun nonterminal -> DStart nonterminal)) _2 )
# 360 "parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Stretch.ocamltype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'actuals) in
    Obj.repr(
# 97 "parser.mly"
    ( List.map (Positions.map (fun nt -> DType (_2, nt)))
        (List.map Parameters.with_pos _3) )
# 369 "parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Stretch.ocamltype) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'nonterminals) in
    Obj.repr(
# 102 "parser.mly"
    ( Misc.mapd (fun ntloc ->
        Positions.mapd (fun nt -> DStart nt, DType (_2, ParameterVar ntloc)) ntloc) _3 )
# 378 "parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'priority_keyword) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'symbols) in
    Obj.repr(
# 106 "parser.mly"
    ( let prec = ParserAux.new_precedence_level (rhs_start_pos 1) (rhs_end_pos 1) in
      List.map (Positions.map (fun symbol -> DTokenProperties (symbol, _1, prec))) _2 )
# 387 "parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Stretch.ocamltype) in
    Obj.repr(
# 110 "parser.mly"
    ( [ unknown_pos (DParameter _2) ] )
# 394 "parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Syntax.attribute) in
    Obj.repr(
# 113 "parser.mly"
    ( [ unknown_pos (DGrammarAttribute _1) ] )
# 401 "parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'actuals) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'attributes) in
    Obj.repr(
# 116 "parser.mly"
    ( [ unknown_pos (DSymbolAttributes (_2, _3)) ] )
# 409 "parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'actuals) in
    Obj.repr(
# 119 "parser.mly"
    ( let prec = ParserAux.new_on_error_reduce_level() in
      List.map (Positions.map (fun nt -> DOnErrorReduce (nt, prec)))
        (List.map Parameters.with_pos _2) )
# 418 "parser.ml"
               : 'declaration))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "parser.mly"
    ( None )
# 424 "parser.ml"
               : 'optional_ocamltype))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Stretch.ocamltype) in
    Obj.repr(
# 127 "parser.mly"
    ( Some _1 )
# 431 "parser.ml"
               : 'optional_ocamltype))
; (fun __caml_parser_env ->
    Obj.repr(
# 131 "parser.mly"
    ( LeftAssoc )
# 437 "parser.ml"
               : 'priority_keyword))
; (fun __caml_parser_env ->
    Obj.repr(
# 133 "parser.mly"
    ( RightAssoc )
# 443 "parser.ml"
               : 'priority_keyword))
; (fun __caml_parser_env ->
    Obj.repr(
# 135 "parser.mly"
    ( NonAssoc )
# 449 "parser.ml"
               : 'priority_keyword))
; (fun __caml_parser_env ->
    Obj.repr(
# 147 "parser.mly"
    ( [] )
# 455 "parser.ml"
               : 'symbols))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'symbols) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'optional_comma) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'symbol) in
    Obj.repr(
# 149 "parser.mly"
    ( _3 :: _1 )
# 464 "parser.ml"
               : 'symbols))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Positions.located) in
    Obj.repr(
# 153 "parser.mly"
    ( _1 )
# 471 "parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string Positions.located) in
    Obj.repr(
# 155 "parser.mly"
    ( _1 )
# 478 "parser.ml"
               : 'symbol))
; (fun __caml_parser_env ->
    Obj.repr(
# 159 "parser.mly"
    ( () )
# 484 "parser.ml"
               : 'optional_comma))
; (fun __caml_parser_env ->
    Obj.repr(
# 161 "parser.mly"
    ( () )
# 490 "parser.ml"
               : 'optional_comma))
; (fun __caml_parser_env ->
    Obj.repr(
# 165 "parser.mly"
    ( [] )
# 496 "parser.ml"
               : 'attributes))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Syntax.attribute) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'attributes) in
    Obj.repr(
# 166 "parser.mly"
                       ( _1 :: _2 )
# 504 "parser.ml"
               : 'attributes))
; (fun __caml_parser_env ->
    Obj.repr(
# 174 "parser.mly"
    ( [] )
# 510 "parser.ml"
               : 'terminals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'terminals) in
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'optional_comma) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string Positions.located) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributes) in
    Obj.repr(
# 176 "parser.mly"
    ( (Positions.map (fun uid -> (uid, _4)) _3) :: _1 )
# 520 "parser.ml"
               : 'terminals))
; (fun __caml_parser_env ->
    Obj.repr(
# 180 "parser.mly"
    ( [] )
# 526 "parser.ml"
               : 'nonterminals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'nonterminals) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : string Positions.located) in
    Obj.repr(
# 182 "parser.mly"
    ( _2 :: _1 )
# 534 "parser.ml"
               : 'nonterminals))
; (fun __caml_parser_env ->
    Obj.repr(
# 191 "parser.mly"
    ( [] )
# 540 "parser.ml"
               : 'rules))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'rules) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'rule) in
    Obj.repr(
# 193 "parser.mly"
    ( _2 :: _1 )
# 548 "parser.ml"
               : 'rules))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'flags) in
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'symbol) in
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'attributes) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'optional_formal_parameters) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'optional_bar) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'production_group) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'production_groups) in
    Obj.repr(
# 203 "parser.mly"
    (
      let public, inline = _1 in
      { pr_public_flag = public;
        pr_inline_flag = inline;
        pr_nt          = Positions.value _2;
        pr_positions   = [ Positions.position _2 ];
        pr_attributes  = _3;
        pr_parameters  = _4;
        pr_branches    = List.flatten (_7 :: List.rev _8)
      }
    )
# 571 "parser.ml"
               : 'rule))
; (fun __caml_parser_env ->
    Obj.repr(
# 217 "parser.mly"
    ( false, false )
# 577 "parser.ml"
               : 'flags))
; (fun __caml_parser_env ->
    Obj.repr(
# 219 "parser.mly"
    ( true, false )
# 583 "parser.ml"
               : 'flags))
; (fun __caml_parser_env ->
    Obj.repr(
# 221 "parser.mly"
    ( false, true )
# 589 "parser.ml"
               : 'flags))
; (fun __caml_parser_env ->
    Obj.repr(
# 223 "parser.mly"
    ( true, true )
# 595 "parser.ml"
               : 'flags))
; (fun __caml_parser_env ->
    Obj.repr(
# 225 "parser.mly"
    ( true, true )
# 601 "parser.ml"
               : 'flags))
; (fun __caml_parser_env ->
    Obj.repr(
# 235 "parser.mly"
    ( [] )
# 607 "parser.ml"
               : 'optional_formal_parameters))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'formal_parameters) in
    Obj.repr(
# 237 "parser.mly"
    ( _2 )
# 614 "parser.ml"
               : 'optional_formal_parameters))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'symbol) in
    Obj.repr(
# 241 "parser.mly"
    ( [ Positions.value _1 ] )
# 621 "parser.ml"
               : 'formal_parameters))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'symbol) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'formal_parameters) in
    Obj.repr(
# 243 "parser.mly"
    ( Positions.value _1 :: _3 )
# 629 "parser.ml"
               : 'formal_parameters))
; (fun __caml_parser_env ->
    Obj.repr(
# 247 "parser.mly"
    ( [] )
# 635 "parser.ml"
               : 'optional_actuals))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'actuals_comma) in
    Obj.repr(
# 249 "parser.mly"
    ( _2 )
# 642 "parser.ml"
               : 'optional_actuals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'actual) in
    Obj.repr(
# 253 "parser.mly"
    ( [ _1 ] )
# 649 "parser.ml"
               : 'actuals_comma))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actual) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'actuals_comma) in
    Obj.repr(
# 255 "parser.mly"
    ( _1 :: _3 )
# 657 "parser.ml"
               : 'actuals_comma))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'symbol) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'optional_actuals) in
    Obj.repr(
# 259 "parser.mly"
    ( Parameters.app _1 _2 )
# 665 "parser.ml"
               : 'actual))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'actual) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'modifier) in
    Obj.repr(
# 261 "parser.mly"
    ( ParameterApp (_2, [ _1 ]) )
# 673 "parser.ml"
               : 'actual))
; (fun __caml_parser_env ->
    Obj.repr(
# 265 "parser.mly"
    ( [] )
# 679 "parser.ml"
               : 'actuals))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'actuals) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'optional_comma) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'actual) in
    Obj.repr(
# 267 "parser.mly"
    ( _3::_1 )
# 688 "parser.ml"
               : 'actuals))
; (fun __caml_parser_env ->
    Obj.repr(
# 271 "parser.mly"
    ( () )
# 694 "parser.ml"
               : 'optional_bar))
; (fun __caml_parser_env ->
    Obj.repr(
# 273 "parser.mly"
    ( () )
# 700 "parser.ml"
               : 'optional_bar))
; (fun __caml_parser_env ->
    Obj.repr(
# 281 "parser.mly"
    ( unknown_pos "option" )
# 706 "parser.ml"
               : 'modifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 283 "parser.mly"
    ( unknown_pos "nonempty_list" )
# 712 "parser.ml"
               : 'modifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 285 "parser.mly"
    ( unknown_pos "list" )
# 718 "parser.ml"
               : 'modifier))
; (fun __caml_parser_env ->
    Obj.repr(
# 293 "parser.mly"
    ( [] )
# 724 "parser.ml"
               : 'production_groups))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'production_groups) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'production_group) in
    Obj.repr(
# 295 "parser.mly"
    ( _3 :: _1 )
# 732 "parser.ml"
               : 'production_groups))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'productions) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Syntax.identifier option array -> Syntax.action) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'optional_precedence) in
    Obj.repr(
# 299 "parser.mly"
    (
      let productions, action, oprec2 = _1, _2, _3 in
      (* If multiple productions share a single semantic action, check
         that all of them bind the same names. *)
      ParserAux.check_production_group productions;
      (* Then, *)
      List.map (fun (producers, oprec1, level, pos) ->
        (* Replace [$i] with [_i]. *)
        let pr_producers = ParserAux.normalize_producers producers in
        (* Distribute the semantic action. Also, check that every [$i]
           is within bounds. *)
        let pr_action = action (ParserAux.producer_names producers) in
        {
          pr_producers;
          pr_action;
          pr_branch_prec_annotation   = ParserAux.override pos oprec1 oprec2;
          pr_branch_production_level  = level;
          pr_branch_position          = pos
        })
      productions
    )
# 761 "parser.ml"
               : 'production_group))
; (fun __caml_parser_env ->
    Obj.repr(
# 323 "parser.mly"
    ( None )
# 767 "parser.ml"
               : 'optional_precedence))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'symbol) in
    Obj.repr(
# 325 "parser.mly"
    ( Some _2 )
# 774 "parser.ml"
               : 'optional_precedence))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : ParserAux.early_production) in
    Obj.repr(
# 334 "parser.mly"
    ( [ _1 ] )
# 781 "parser.ml"
               : 'productions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : ParserAux.early_production) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'bar_productions) in
    Obj.repr(
# 336 "parser.mly"
    ( _1 :: _2 )
# 789 "parser.ml"
               : 'productions))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : ParserAux.early_production) in
    Obj.repr(
# 340 "parser.mly"
    ( [ _2 ] )
# 796 "parser.ml"
               : 'bar_productions))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : ParserAux.early_production) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'bar_productions) in
    Obj.repr(
# 342 "parser.mly"
    ( _2 :: _3 )
# 804 "parser.ml"
               : 'bar_productions))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'producers) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'optional_precedence) in
    Obj.repr(
# 346 "parser.mly"
    ( List.rev _1,
      _2,
      ParserAux.new_production_level(),
      Positions.lex_join (symbol_start_pos()) (symbol_end_pos())
    )
# 816 "parser.ml"
               : ParserAux.early_production))
; (fun __caml_parser_env ->
    Obj.repr(
# 354 "parser.mly"
    ( [] )
# 822 "parser.ml"
               : 'producers))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'producers) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : ParserAux.early_producer) in
    Obj.repr(
# 356 "parser.mly"
    ( _2 :: _1 )
# 830 "parser.ml"
               : 'producers))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'actual) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'attributes) in
    Obj.repr(
# 364 "parser.mly"
    ( Positions.lex_join (symbol_start_pos()) (symbol_end_pos()),    None, _1, _2 )
# 838 "parser.ml"
               : ParserAux.early_producer))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string Positions.located) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'actual) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : 'attributes) in
    Obj.repr(
# 366 "parser.mly"
    ( Positions.lex_join (symbol_start_pos()) (symbol_end_pos()), Some _1, _3, _4 )
# 847 "parser.ml"
               : ParserAux.early_producer))
(* Entry grammar *)
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
let grammar (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Syntax.partial_grammar)
;;
