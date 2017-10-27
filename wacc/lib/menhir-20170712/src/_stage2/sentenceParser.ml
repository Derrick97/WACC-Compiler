
(* This generated code requires the following version of MenhirLib: *)

let () =
  MenhirLib.StaticVersion.require_20170712

module MenhirBasics = struct
  
  exception Error = Parsing.Parse_error
  
  type token = 
    | TERMINAL of (
# 29 "sentenceParser.mly"
      (Grammar.Terminal.t * Lexing.position * Lexing.position)
# 16 "sentenceParser.ml"
  )
    | NONTERMINAL of (
# 30 "sentenceParser.mly"
      (Grammar.Nonterminal.t * Lexing.position * Lexing.position)
# 21 "sentenceParser.ml"
  )
    | EOL
    | EOF
    | COMMENT of (
# 31 "sentenceParser.mly"
      (string)
# 28 "sentenceParser.ml"
  )
    | COLON
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

# 37 "sentenceParser.mly"
  

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


# 76 "sentenceParser.ml"

module Tables = struct
  
  include MenhirBasics
  
  let token2terminal : token -> int =
    fun _tok ->
      match _tok with
      | COLON ->
          6
      | COMMENT _ ->
          5
      | EOF ->
          4
      | EOL ->
          3
      | NONTERMINAL _ ->
          2
      | TERMINAL _ ->
          1
  
  and error_terminal =
    0
  
  and token2value : token -> Obj.t =
    fun _tok ->
      match _tok with
      | COLON ->
          Obj.repr ()
      | COMMENT _v ->
          Obj.repr _v
      | EOF ->
          Obj.repr ()
      | EOL ->
          Obj.repr ()
      | NONTERMINAL _v ->
          Obj.repr _v
      | TERMINAL _v ->
          Obj.repr _v
  
  and default_reduction =
    (4, "\000\208\000\160\011G\006\003\016\000\n\128\185 ")
  
  and error =
    (7, "|\160\000\021\002\000>\016\000\000\007\192\002\000\000\240\n\129\000\000\b\000\000\000")
  
  and start =
    2
  
  and action =
    ((8, "\003\003\000\006\003\014\000\003\016\000\000\000\003\000\016\000\000\b\014\003\022\000\000\024\000\000\000"), (8, "\006\014-\017\030\006J-[\018\026&?NWc"))
  
  and lhs =
    (4, "\016vUTC2 ")
  
  and goto =
    ((8, "\005\000\000\000\012\000\000\b\000\000\000\000\018\000\000\000\000\028\000\022\000\000\000\000\000\000\000"), (8, "\t\011\003\015\r\017\t\011\006\012\r\t\011\021\014\r\024\026\027"))
  
  and semantic_action =
    [|
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.semv = _2;
          MenhirLib.EngineTypes.startp = _startpos__2_;
          MenhirLib.EngineTypes.endp = _endpos__2_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = _1;
            MenhirLib.EngineTypes.startp = _startpos__1_;
            MenhirLib.EngineTypes.endp = _endpos__1_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let _2 : unit = Obj.magic _2 in
        let _1 : (SentenceParserAux.located_sentence SentenceParserAux.or_comment list) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 79 "sentenceParser.mly"
     (SentenceParserAux.located_sentence SentenceParserAux.or_comment list)
# 159 "sentenceParser.ml"
        ) = 
# 88 "sentenceParser.mly"
  ( _1 )
# 163 "sentenceParser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = _1;
          MenhirLib.EngineTypes.startp = _startpos__1_;
          MenhirLib.EngineTypes.endp = _endpos__1_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : ((Grammar.Nonterminal.t * Lexing.position * Lexing.position) option *
  (Grammar.Terminal.t * Lexing.position * Lexing.position) list) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 74 "sentenceParser.mly"
      (located_sentence)
# 189 "sentenceParser.ml"
        ) = 
# 98 "sentenceParser.mly"
  ( locate_sentence _1 )
# 193 "sentenceParser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.MenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : (SentenceParserAux.located_sentence SentenceParserAux.or_comment list) = 
# 92 "sentenceParser.mly"
  ( [] )
# 211 "sentenceParser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.semv = _2;
          MenhirLib.EngineTypes.startp = _startpos__2_;
          MenhirLib.EngineTypes.endp = _endpos__2_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = _1;
            MenhirLib.EngineTypes.startp = _startpos__1_;
            MenhirLib.EngineTypes.endp = _endpos__1_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let _2 : (SentenceParserAux.located_sentence SentenceParserAux.or_comment list) = Obj.magic _2 in
        let _1 : (
# 74 "sentenceParser.mly"
      (located_sentence)
# 238 "sentenceParser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (SentenceParserAux.located_sentence SentenceParserAux.or_comment list) = 
# 93 "sentenceParser.mly"
                                                 ( Thing   _1 :: _2 )
# 246 "sentenceParser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.semv = _2;
          MenhirLib.EngineTypes.startp = _startpos__2_;
          MenhirLib.EngineTypes.endp = _endpos__2_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = _1;
            MenhirLib.EngineTypes.startp = _startpos__1_;
            MenhirLib.EngineTypes.endp = _endpos__1_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let _2 : (SentenceParserAux.located_sentence SentenceParserAux.or_comment list) = Obj.magic _2 in
        let _1 : (
# 31 "sentenceParser.mly"
      (string)
# 273 "sentenceParser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (SentenceParserAux.located_sentence SentenceParserAux.or_comment list) = 
# 94 "sentenceParser.mly"
                                                 ( Comment _1 :: _2 )
# 281 "sentenceParser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = _1;
          MenhirLib.EngineTypes.startp = _startpos__1_;
          MenhirLib.EngineTypes.endp = _endpos__1_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 76 "sentenceParser.mly"
      (SentenceParserAux.sentence option)
# 306 "sentenceParser.ml"
        ) = 
# 103 "sentenceParser.mly"
    ( None )
# 310 "sentenceParser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = _1;
          MenhirLib.EngineTypes.startp = _startpos__1_;
          MenhirLib.EngineTypes.endp = _endpos__1_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : ((Grammar.Nonterminal.t * Lexing.position * Lexing.position) option *
  (Grammar.Terminal.t * Lexing.position * Lexing.position) list) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 76 "sentenceParser.mly"
      (SentenceParserAux.sentence option)
# 336 "sentenceParser.ml"
        ) = 
# 105 "sentenceParser.mly"
    ( Some (strip_sentence _1) )
# 340 "sentenceParser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.semv = _4;
          MenhirLib.EngineTypes.startp = _startpos__4_;
          MenhirLib.EngineTypes.endp = _endpos__4_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = _3;
            MenhirLib.EngineTypes.startp = _startpos__3_;
            MenhirLib.EngineTypes.endp = _endpos__3_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.semv = _2;
              MenhirLib.EngineTypes.startp = _startpos__2_;
              MenhirLib.EngineTypes.endp = _endpos__2_;
              MenhirLib.EngineTypes.next = {
                MenhirLib.EngineTypes.state = _menhir_s;
                MenhirLib.EngineTypes.semv = _1;
                MenhirLib.EngineTypes.startp = _startpos__1_;
                MenhirLib.EngineTypes.endp = _endpos__1_;
                MenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let _4 : unit = Obj.magic _4 in
        let _3 : ((Grammar.Terminal.t * Lexing.position * Lexing.position) list) = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let _1 : (
# 30 "sentenceParser.mly"
      (Grammar.Nonterminal.t * Lexing.position * Lexing.position)
# 379 "sentenceParser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : ((Grammar.Nonterminal.t * Lexing.position * Lexing.position) option *
  (Grammar.Terminal.t * Lexing.position * Lexing.position) list) = 
# 111 "sentenceParser.mly"
    ( Some _1, _3 )
# 388 "sentenceParser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.semv = _2;
          MenhirLib.EngineTypes.startp = _startpos__2_;
          MenhirLib.EngineTypes.endp = _endpos__2_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = _1;
            MenhirLib.EngineTypes.startp = _startpos__1_;
            MenhirLib.EngineTypes.endp = _endpos__1_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let _2 : unit = Obj.magic _2 in
        let _1 : ((Grammar.Terminal.t * Lexing.position * Lexing.position) list) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : ((Grammar.Nonterminal.t * Lexing.position * Lexing.position) option *
  (Grammar.Terminal.t * Lexing.position * Lexing.position) list) = 
# 113 "sentenceParser.mly"
    ( None, _1 )
# 420 "sentenceParser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.MenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : ((Grammar.Terminal.t * Lexing.position * Lexing.position) list) = 
# 118 "sentenceParser.mly"
    ( [] )
# 438 "sentenceParser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.semv = _2;
          MenhirLib.EngineTypes.startp = _startpos__2_;
          MenhirLib.EngineTypes.endp = _endpos__2_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = _1;
            MenhirLib.EngineTypes.startp = _startpos__1_;
            MenhirLib.EngineTypes.endp = _endpos__1_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let _2 : ((Grammar.Terminal.t * Lexing.position * Lexing.position) list) = Obj.magic _2 in
        let _1 : (
# 29 "sentenceParser.mly"
      (Grammar.Terminal.t * Lexing.position * Lexing.position)
# 465 "sentenceParser.ml"
        ) = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : ((Grammar.Terminal.t * Lexing.position * Lexing.position) list) = 
# 120 "sentenceParser.mly"
    ( _1 :: _2 )
# 473 "sentenceParser.ml"
         in
        {
          MenhirLib.EngineTypes.state = _menhir_s;
          MenhirLib.EngineTypes.semv = Obj.repr _v;
          MenhirLib.EngineTypes.startp = _startpos;
          MenhirLib.EngineTypes.endp = _endpos;
          MenhirLib.EngineTypes.next = _menhir_stack;
        });
    |]
  
  and trace =
    None
  
end

module MenhirInterpreter = struct
  
  module ET = MenhirLib.TableInterpreter.MakeEngineTable (Tables)
  
  module TI = MenhirLib.Engine.Make (ET)
  
  include TI
  
end

let optional_sentence =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 17 lexer lexbuf) : (
# 76 "sentenceParser.mly"
      (SentenceParserAux.sentence option)
# 504 "sentenceParser.ml"
    ))

and entry =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 0 lexer lexbuf) : (
# 79 "sentenceParser.mly"
     (SentenceParserAux.located_sentence SentenceParserAux.or_comment list)
# 512 "sentenceParser.ml"
    ))

module Incremental = struct
  
  let optional_sentence =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 17 initial_position) : (
# 76 "sentenceParser.mly"
      (SentenceParserAux.sentence option)
# 522 "sentenceParser.ml"
      ) MenhirInterpreter.checkpoint)
  
  and entry =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 0 initial_position) : (
# 79 "sentenceParser.mly"
     (SentenceParserAux.located_sentence SentenceParserAux.or_comment list)
# 530 "sentenceParser.ml"
      ) MenhirInterpreter.checkpoint)
  
end

# 219 "../standard.mly"
  


# 539 "sentenceParser.ml"
