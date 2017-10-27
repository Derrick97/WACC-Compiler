
(* This generated code requires the following version of MenhirLib: *)

let () =
  MenhirLib.StaticVersion.require_20170712

module MenhirBasics = struct
  
  exception Error = Parsing.Parse_error
  
  type token = 
    | UID of (
# 36 "parser.mly"
       (string Positions.located)
# 16 "parser.ml"
  )
    | TYPE
    | TOKEN
    | START
    | STAR
    | RPAREN
    | RIGHT
    | QUESTION
    | PUBLIC
    | PREC
    | PLUS
    | PERCENTPERCENT of (
# 39 "parser.mly"
       (Stretch.t Lazy.t)
# 31 "parser.ml"
  )
    | PERCENTATTRIBUTE
    | PARAMETER
    | ON_ERROR_REDUCE
    | OCAMLTYPE of (
# 38 "parser.mly"
       (Stretch.ocamltype)
# 39 "parser.ml"
  )
    | NONASSOC
    | LPAREN
    | LID of (
# 36 "parser.mly"
       (string Positions.located)
# 46 "parser.ml"
  )
    | LEFT
    | INLINE
    | HEADER of (
# 37 "parser.mly"
       (Stretch.t)
# 53 "parser.ml"
  )
    | GRAMMARATTRIBUTE of (
# 41 "parser.mly"
       (Syntax.attribute)
# 58 "parser.ml"
  )
    | EQUAL
    | EOF
    | COMMA
    | COLON
    | BAR
    | ATTRIBUTE of (
# 41 "parser.mly"
       (Syntax.attribute)
# 68 "parser.ml"
  )
    | ACTION of (
# 40 "parser.mly"
       (Syntax.identifier option array -> Action.t)
# 73 "parser.ml"
  )
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

# 24 "parser.mly"
  

open Syntax
open Positions


# 90 "parser.ml"

module Tables = struct
  
  include MenhirBasics
  
  let token2terminal : token -> int =
    fun _tok ->
      match _tok with
      | ACTION _ ->
          30
      | ATTRIBUTE _ ->
          29
      | BAR ->
          28
      | COLON ->
          27
      | COMMA ->
          26
      | EOF ->
          25
      | EQUAL ->
          24
      | GRAMMARATTRIBUTE _ ->
          23
      | HEADER _ ->
          22
      | INLINE ->
          21
      | LEFT ->
          20
      | LID _ ->
          19
      | LPAREN ->
          18
      | NONASSOC ->
          17
      | OCAMLTYPE _ ->
          16
      | ON_ERROR_REDUCE ->
          15
      | PARAMETER ->
          14
      | PERCENTATTRIBUTE ->
          13
      | PERCENTPERCENT _ ->
          12
      | PLUS ->
          11
      | PREC ->
          10
      | PUBLIC ->
          9
      | QUESTION ->
          8
      | RIGHT ->
          7
      | RPAREN ->
          6
      | STAR ->
          5
      | START ->
          4
      | TOKEN ->
          3
      | TYPE ->
          2
      | UID _ ->
          1
  
  and error_terminal =
    0
  
  and token2value : token -> Obj.t =
    fun _tok ->
      match _tok with
      | ACTION _v ->
          Obj.repr _v
      | ATTRIBUTE _v ->
          Obj.repr _v
      | BAR ->
          Obj.repr ()
      | COLON ->
          Obj.repr ()
      | COMMA ->
          Obj.repr ()
      | EOF ->
          Obj.repr ()
      | EQUAL ->
          Obj.repr ()
      | GRAMMARATTRIBUTE _v ->
          Obj.repr _v
      | HEADER _v ->
          Obj.repr _v
      | INLINE ->
          Obj.repr ()
      | LEFT ->
          Obj.repr ()
      | LID _v ->
          Obj.repr _v
      | LPAREN ->
          Obj.repr ()
      | NONASSOC ->
          Obj.repr ()
      | OCAMLTYPE _v ->
          Obj.repr _v
      | ON_ERROR_REDUCE ->
          Obj.repr ()
      | PARAMETER ->
          Obj.repr ()
      | PERCENTATTRIBUTE ->
          Obj.repr ()
      | PERCENTPERCENT _v ->
          Obj.repr _v
      | PLUS ->
          Obj.repr ()
      | PREC ->
          Obj.repr ()
      | PUBLIC ->
          Obj.repr ()
      | QUESTION ->
          Obj.repr ()
      | RIGHT ->
          Obj.repr ()
      | RPAREN ->
          Obj.repr ()
      | STAR ->
          Obj.repr ()
      | START ->
          Obj.repr ()
      | TOKEN ->
          Obj.repr ()
      | TYPE ->
          Obj.repr ()
      | UID _v ->
          Obj.repr _v
  
  and default_reduction =
    (8, "\000\000\000TS\000\000TS\000\000\000*()\000FR\000%Q\000%Q\000*().\000LR\007\0000\000\000\000\027\000.\000P\005\0000\000\000.\000J\0066\r\000TS\000\000\000%Q\000*()\000LR\000\000,\011\000\t\000\01275\014\004\n\016\015\000TS\000\000N\b\000\000\000\020\000\021\000!\000TS\000\000\027\000\000TS\000\000H\000'\000\0002\000T\000\000S\000\000T\000\000\000\000#\025\000\000\000TS=\000\000B\000\000@\000\031\000*()\000\027\0038\000\000TS;\000\000D\000*()\000\000\000#\002\0009>\000\000\000TS=\000\000B\00043\022\001\000\029")
  
  and error =
    (31, "9OOP\000\001\000\001\000\000@\000\000\000\000\000\000\000\000\015\187\239\238\016\000\004\000\000\000\000\000\000\000\000\000\r @A\000\000@\0004\128\001\000\000\000\000\000\000\000\000\000\000\000\000 \000\b\000\000\000\000\000\000\000\000\000\b\000\000\000\000\000\000\000\000\000\000\000@\000\000\000\000\000\000\000\000\000\000}\223_p\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\b\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\128\001\000\000\000\000\000\002\000\000\000\007\148\244\247O)\233\238\128\000\000\000<\167\167\184\000\000\000\000\128\000\000\000\000\000\000\000\000\000\000\000\000\t\000\000\000\000\000\000\000\004\000\028\167\175\184\000\000\000\000\000\000 \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\004\000\000\000\000\000\000\000\000\000\137 `I\000\000@\000\016\000\000\000\000\000\000\000\000\000\000\017$\004\t\000\000\000\000\000\000\000\000\000\000\000\001\000\000@\000\000\000\000\000\000\000\000\000\000\000\000\142S\211\213\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\002\000\000\128\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\001\000\000\000\000\000\000\000\000\000<\167\175\184@\000\016\000\000\000\000\000\000\000\000\000\000@\000\004\004\129D\b\000\002\128\000\000\000\000  \b\000\000\000\000\000\128\144(\128\000\000\000\002\000\000\128\000\000\000\000\000\000\000\000\000\000\b\005\000\000\016\n\000\000\000\000\000\000@!\000\000@\000\000\000\000\000\000\000\000\000@\000\004\016\000\004\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000B\001\000\128P\000\000\000\b\004\002\001@\000\000\000\"X\024G@\000\016\000\000\000\000\001\018\192\192:\001\000\128P\000\000\000\b\214\006\021\209\172\012\011\160\016\b\005\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\"\000\002\136\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\000Q\000\128@(\000\000\000\000\000\000\000\168\004\002\001@\000\000\000 \016\b\005\000\000\000\000\137` \028\000\000\000\000\000\000\000\000\000\000\000\b\004\002\001\192\000\000\000\000\000\000\000\000\000\000\000\000@\000\021\000\000@\000\000\000\000\000\000\000\000\000\000\000\000\000\128\000\b \016\b\005\000\000\000\000\137` \028\000\000\000\000\000\000\000\000\000\000\000\b\214\002\005\209\172\004\011\129\000\000\000\000\000\000\000\000\000\000\001\018\192@8\000\000\000\000\000\000\000\000\000\000\000P\026\005\018 \000\b\000\000\000\000\000\000\000\000\000\000\000\000\002\002@\162D\002\001\000\160\000\000\000\000\002\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\202zz\128\000\000\000\000")
  
  and start =
    1
  
  and action =
    ((16, "\001z\000\015\000\174\000\000\000\000\000\003\000\254\000\000\000\000\000\007\000\254\001B\000\000\000\000\000\000\000\254\000\000\000\000\0004\000\000\000\000\000<\000\000\000\000\0004\000\000\000\000\000\000\000\000\000\174\000\000\000\000\000\000\000\198\000\000\000R\000j\000j\000\000\000\218\000\000\000R\000\000\000\000\000\196\000\000\000\148\001\014\000\000\000\148\000\000\000\000\000\000\000\000\001\024\000\000\000\000\002\194\000\254\000\184\000\000\000\000\002\232\000\000\000\000\000\000\001\024\000\000\000\000\000\180\001B\000\000\000\000\000\222\000\000\000\174\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\158\000\000\000\000\000\164\001\158\000\000\000\000\001N\003\024\000\248\000\000\000\136\000\000\003\024\000\000\001\172\000\000\000\000\000@\000@\000\000\0012\001\180\000\000\000\000\000L\001\180\000\000\001x\000\000\001P\000|\000\000\003\016\000\000\001\168\001\196\000\000\002\\\003\020\000\000\001\168\001\220\003\020\001\132\000\000\000\000\001~\001f\001\200\000\000\000\000\000\000\001V\003\016\000\000\001\152\003\016\000\000\003\016\000\000\002t\000\000\000\000\000\000\002\028\000\000\000\000\000\000\000\246\001\212\000\000\000\000\000\000\000\228\003\020\000\000\002\168\000\000\000\000\000\000\002\016\002(\001\180\000\000\000\000\002t\000\000\000\000\001\134\002\144\001\252\000\000\000\000\000\000\002\238\003\016\000\000\000\\\000\000\000\000\000\000\000\000\001z\000\000"), (16, "\000\141\000\141\000\141\000\141\000\141\000\141\000\141\000\141\000\141\000\n\000\141\000\141\000\141\000\141\000\141\000*\000\141\000\026\000\141\000\141\000\141\000\141\000\141\000\141\000\141\000\141\000\141\000\177\001)\001)\001)\000f\000N\001)\000j\001)\000Z\000n\001)\001)\001)\001)\000\146\001)\001\025\000\177\001)\001)\001)\001)\000e\001)\000r\001)\000e\000e\000e\000e\002\223\000e\000e\001\158\000e\000\193\001\186\000e\000e\000e\000e\000I\000e\002\227\000\193\000e\000e\000e\000e\001\130\000e\000e\000e\000\193\000\150\000\177\0011\0011\0011\000I\000\014\0011\001\210\0011\000\193\000\190\0011\0011\0011\0011\000\242\0011\000\185\000\177\0011\0011\0011\0011\000\018\0011\000r\0011\000\177\0019\0019\0019\000\182\000\138\0019\000\185\0019\001\026\001\t\0019\0019\0019\0019\000E\0019\001*\000\030\0019\0019\0019\0019\002j\0019\000\162\0019\001!\001!\001!\002~\000\222\001!\000E\001!\001z\000\"\001!\001!\001!\001!\000\229\001!\000\229\000\177\001!\001!\001!\001!\000\226\001!\000\194\001!\000\169\000\169\000\169\0002\001\017\000\169\0006\000\169\001\170\000:\000\169\000\169\000\169\000\169\001\001\000\169\001r\000\149\000\169\000\169\000\169\000\169\000\237\000\169\000>\000\169\002\022\001\026\000\006\000\134\000\178\001\198\001\206\000\210\001\001\000\214\002*\002\006\000m\000\218\001&\001.\000\237\0016\000\237\001V\001:\001>\001B\001F\001I\001J\001\146\001N\001I\001I\001\174\001I\002\018\001I\001I\002\162\002\186\001Z\001\218\000\000\002\026\001I\001I\0026\001\150\000\249\002n\001\226\001\178\001I\000\133\001I\001I\001I\000\133\000\133\001\230\000\133\002\030\000\133\000\133\000\000\000\000\000\000\002r\000\000\002\194\001\254\000\133\000\000\000\000\000\000\000\000\000\000\000\000\000\133\000\t\000\133\000\133\000\133\000\t\000]\000e\000\t\002\198\000\t\000\t\000\000\000\005\000\000\000\000\000e\000\005\000Y\000\t\000\005\000\000\000\005\000\005\000\000\000e\000]\000\000\000\t\000\t\000\t\000\005\000\000\000\000\000e\002V\000e\000\000\000Y\000\133\000\005\000\005\000\005\000\133\000\000\000\000\000\133\000\000\000\133\000\133\000\000\000e\000\000\000\000\000\000\002J\001\238\000\133\002N\000\000\000e\002R\000\000\000\000\000\000\000\237\000\133\000\133\000\133\000e\000\000\000\000\000\000\000\237\002\190\000\000\000\237\000e\000e\002V\000e\002\138\000\000\000\237\002\142\000\237\000e\002\146\000\000\000\237\000\141\000\000\000\237\000\000\000\141\000e\000\000\000\141\000\000\000\000\000\141\000\000\000\000\000\000\000e\002V\000e\000\234\000\141\000\177\000\000\000\000\001\001\000\254\000\000\000\141\001\002\000\000\000\141\001\006\001\001\000\000\000\000\001\001\000\000\000\000\000\000\000\177\000\000\001\218\001\001\001\242\001\001\000A\000r\000\000\001\001\001)\000u\002\210\000u\001v\000\000\000\000\000}\000\000\000\000\001\222\000\000\001\246\000\000\000A\000\000\001~\000\000\000\000\000u\000}\000u\000\000\000u"))
  
  and lhs =
    (8, "\000\"\"!!!!!!!!!!!!!     \031\030\030\030\029\029\028\028\027\027\026\026\025\025\024\024\023\023\022\022\022\021\021\020\020\019\019\018\018\017\017\016\016\016\015\015\014\014\r\r\012\011\011\n\n\t\t\b\b\007\007\006\006\005\005\004\004\003\003\002\002\001\001")
  
  and goto =
    ((16, "\000\130\000\000\000\000\000\000\000\000\000%\000J\000\000\000\000\000\006\000R\000\012\000\000\000\000\000\000\000\196\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\011\000\000\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000,\000\000\000^\0002\000:\000\000\000h\000\000\000\158\000\000\000\000\000\146\000\000\000\240\000\214\000\000\000\246\000\000\000\000\000\000\000\000\000\204\000\000\000\000\000\218\000\216\000\000\000\000\000\000\000\t\000\000\000\000\000\000\000\220\000\000\000\000\000\228\000\230\000\000\000\000\000\000\000\000\000\224\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\134\000\000\000\000\000\234\000\180\000\000\000\000\000\000\000\186\000\000\000\000\000\000\000\000\000\212\000\000\001\022\000\000\000\000\000\224\000\226\000\000\000\240\000Z\000\000\000\000\000\000\000\138\000\000\000\000\000\000\000\000\000\252\000\000\000\\\000\000\000\000\000\238\000\000\000\240\000\003\000\000\000\000\000\242\000\020\000\000\000\000\000\000\000\000\000\000\001$\000\000\000\000\000\000\000\000\000h\000\000\000\000\000~\000\000\000\136\000\000\000\198\000\000\000\000\000\000\000\238\000\000\000\000\000\000\000\000\001(\000\000\000\000\000\000\000\000\000*\000\000\000\202\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\204\000\000\000\000\000\000\000\000\001*\000\000\000\000\000\000\000\000\000\148\000\000\001\012\000\000\000\000\000\000\000\000\000\212\000\000"), (8, "\127\006\025\006\025!\024\031\168\131\132\127\138\141\144\030C E\129\131\132\127\138\141\144\154\021\018\159\161\131\132\162\138\141\144\154\n\012\159$\n\012\162\022n{\154\019,\159q{(\162\173\174'\180\141\144\140\132{\138\141\144X{n[*\154\143{p\141\144\154\146U+\144\182\174\146\180\141\144\154X/\\Z\145\187\146\188\n\012\154\146:?b\017F\146\n\012:?\006\025D<Mb\183{\152U\166\152d42\1533\153\172c>\189IHYd\188gjisv\170\167\137\151\158\179\186\000\171"))
  
  and semantic_action =
    [|
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.MenhirLib.EngineTypes.stack in
        let {
          MenhirLib.EngineTypes.semv = params00;
          MenhirLib.EngineTypes.startp = _startpos_params00_;
          MenhirLib.EngineTypes.endp = _endpos_params00_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = symbol0;
            MenhirLib.EngineTypes.startp = _startpos_symbol0_;
            MenhirLib.EngineTypes.endp = _endpos_symbol0_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let params00 : (Syntax.parameters) = Obj.magic params00 in
        let symbol0 : (Syntax.terminal Positions.located) = Obj.magic symbol0 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_symbol0_ in
        let _endpos = _endpos_params00_ in
        let _v : (Syntax.parameter) = let p =
          let params0 = params00 in
          let symbol = symbol0 in
          let actuals =
            let params = params0 in
            
# 352 "parser.mly"
    ( params )
# 274 "parser.ml"
            
          in
          
# 319 "parser.mly"
    ( Parameters.app symbol actuals )
# 280 "parser.ml"
          
        in
        
# 330 "parser.mly"
    ( p )
# 286 "parser.ml"
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
          MenhirLib.EngineTypes.semv = m0;
          MenhirLib.EngineTypes.startp = _startpos_m0_;
          MenhirLib.EngineTypes.endp = _endpos_m0_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = p0;
            MenhirLib.EngineTypes.startp = _startpos_p0_;
            MenhirLib.EngineTypes.endp = _endpos_p0_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let m0 : (Syntax.symbol Positions.located) = Obj.magic m0 in
        let p0 : (Syntax.parameter) = Obj.magic p0 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_p0_ in
        let _endpos = _endpos_m0_ in
        let _v : (Syntax.parameter) = let p =
          let m = m0 in
          let p = p0 in
          
# 322 "parser.mly"
    ( ParameterApp (m, [ p ]) )
# 320 "parser.ml"
          
        in
        
# 330 "parser.mly"
    ( p )
# 326 "parser.ml"
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
          MenhirLib.EngineTypes.semv = h;
          MenhirLib.EngineTypes.startp = _startpos_h_;
          MenhirLib.EngineTypes.endp = _endpos_h_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let h : (
# 37 "parser.mly"
       (Stretch.t)
# 347 "parser.ml"
        ) = Obj.magic h in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_h_ in
        let _endpos = _endpos_h_ in
        let _v : (Syntax.declaration Positions.located list) = let _endpos = _endpos_h_ in
        let _startpos = _startpos_h_ in
        
# 88 "parser.mly"
    ( [ with_poss _startpos _endpos (DCode h) ] )
# 357 "parser.ml"
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
          MenhirLib.EngineTypes.semv = xs0;
          MenhirLib.EngineTypes.startp = _startpos_xs0_;
          MenhirLib.EngineTypes.endp = _endpos_xs0_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = ty;
            MenhirLib.EngineTypes.startp = _startpos_ty_;
            MenhirLib.EngineTypes.endp = _endpos_ty_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = _1;
              MenhirLib.EngineTypes.startp = _startpos__1_;
              MenhirLib.EngineTypes.endp = _endpos__1_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs0 : ((Syntax.terminal * Syntax.attributes) Positions.located list) = Obj.magic xs0 in
        let ty : (Stretch.ocamltype option) = Obj.magic ty in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_xs0_ in
        let _v : (Syntax.declaration Positions.located list) = let ts =
          let xs = xs0 in
          
# 158 "parser.mly"
    ( xs )
# 396 "parser.ml"
          
        in
        
# 91 "parser.mly"
    ( List.map (Positions.map (fun (terminal, attrs) -> DToken (ty, terminal, attrs))) ts )
# 402 "parser.ml"
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
          MenhirLib.EngineTypes.semv = xs0;
          MenhirLib.EngineTypes.startp = _startpos_xs0_;
          MenhirLib.EngineTypes.endp = _endpos_xs0_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = t;
            MenhirLib.EngineTypes.startp = _startpos_t_;
            MenhirLib.EngineTypes.endp = _endpos_t_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = _1;
              MenhirLib.EngineTypes.startp = _startpos__1_;
              MenhirLib.EngineTypes.endp = _endpos__1_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs0 : (Syntax.nonterminal Positions.located list) = Obj.magic xs0 in
        let t : (Stretch.ocamltype option) = Obj.magic t in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_xs0_ in
        let _v : (Syntax.declaration Positions.located list) = let nts =
          let xs = xs0 in
          
# 158 "parser.mly"
    ( xs )
# 441 "parser.ml"
          
        in
        
# 95 "parser.mly"
    (
      match t with
      | None ->
          List.map (Positions.map (fun nonterminal -> DStart nonterminal)) nts
      | Some t ->
          Misc.mapd (fun ntloc ->
            Positions.mapd (fun nt -> DStart nt, DType (t, ParameterVar ntloc)) ntloc) nts
    )
# 454 "parser.ml"
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
          MenhirLib.EngineTypes.semv = xs0;
          MenhirLib.EngineTypes.startp = _startpos_xs0_;
          MenhirLib.EngineTypes.endp = _endpos_xs0_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = t;
            MenhirLib.EngineTypes.startp = _startpos_t_;
            MenhirLib.EngineTypes.endp = _endpos_t_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = _1;
              MenhirLib.EngineTypes.startp = _startpos__1_;
              MenhirLib.EngineTypes.endp = _endpos__1_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs0 : (Syntax.parameter list) = Obj.magic xs0 in
        let t : (
# 38 "parser.mly"
       (Stretch.ocamltype)
# 486 "parser.ml"
        ) = Obj.magic t in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_xs0_ in
        let _v : (Syntax.declaration Positions.located list) = let ss =
          let xs = xs0 in
          
# 158 "parser.mly"
    ( xs )
# 497 "parser.ml"
          
        in
        
# 105 "parser.mly"
    ( List.map (Positions.map (fun nt -> DType (t, nt)))
        (List.map Parameters.with_pos ss) )
# 504 "parser.ml"
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
          MenhirLib.EngineTypes.semv = xs0;
          MenhirLib.EngineTypes.startp = _startpos_xs0_;
          MenhirLib.EngineTypes.endp = _endpos_xs0_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = k;
            MenhirLib.EngineTypes.startp = _startpos_k_;
            MenhirLib.EngineTypes.endp = _endpos_k_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let xs0 : (Syntax.terminal Positions.located list) = Obj.magic xs0 in
        let k : (Syntax.token_associativity) = Obj.magic k in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_k_ in
        let _endpos = _endpos_xs0_ in
        let _v : (Syntax.declaration Positions.located list) = let ss =
          let xs = xs0 in
          
# 158 "parser.mly"
    ( xs )
# 537 "parser.ml"
          
        in
        
# 109 "parser.mly"
    ( let prec = ParserAux.new_precedence_level _startpos_k_ _endpos_k_ in
      List.map (Positions.map (fun symbol -> DTokenProperties (symbol, k, prec))) ss )
# 544 "parser.ml"
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
          MenhirLib.EngineTypes.semv = t;
          MenhirLib.EngineTypes.startp = _startpos_t_;
          MenhirLib.EngineTypes.endp = _endpos_t_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = _1;
            MenhirLib.EngineTypes.startp = _startpos__1_;
            MenhirLib.EngineTypes.endp = _endpos__1_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let t : (
# 38 "parser.mly"
       (Stretch.ocamltype)
# 570 "parser.ml"
        ) = Obj.magic t in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_t_ in
        let _v : (Syntax.declaration Positions.located list) = let _endpos = _endpos_t_ in
        let _startpos = _startpos__1_ in
        
# 113 "parser.mly"
    ( [ with_poss _startpos _endpos (DParameter t) ] )
# 581 "parser.ml"
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
          MenhirLib.EngineTypes.semv = attr;
          MenhirLib.EngineTypes.startp = _startpos_attr_;
          MenhirLib.EngineTypes.endp = _endpos_attr_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let attr : (
# 41 "parser.mly"
       (Syntax.attribute)
# 602 "parser.ml"
        ) = Obj.magic attr in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_attr_ in
        let _endpos = _endpos_attr_ in
        let _v : (Syntax.declaration Positions.located list) = let _endpos = _endpos_attr_ in
        let _startpos = _startpos_attr_ in
        
# 116 "parser.mly"
    ( [ with_poss _startpos _endpos (DGrammarAttribute attr) ] )
# 612 "parser.ml"
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
          MenhirLib.EngineTypes.semv = attrs;
          MenhirLib.EngineTypes.startp = _startpos_attrs_;
          MenhirLib.EngineTypes.endp = _endpos_attrs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = xs0;
            MenhirLib.EngineTypes.startp = _startpos_xs0_;
            MenhirLib.EngineTypes.endp = _endpos_xs0_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = _1;
              MenhirLib.EngineTypes.startp = _startpos__1_;
              MenhirLib.EngineTypes.endp = _endpos__1_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let attrs : (Syntax.attributes) = Obj.magic attrs in
        let xs0 : (Syntax.parameter list) = Obj.magic xs0 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_attrs_ in
        let _v : (Syntax.declaration Positions.located list) = let actuals =
          let xs = xs0 in
          
# 158 "parser.mly"
    ( xs )
# 651 "parser.ml"
          
        in
        let _endpos = _endpos_attrs_ in
        let _startpos = _startpos__1_ in
        
# 119 "parser.mly"
    ( [ with_poss _startpos _endpos (DSymbolAttributes (actuals, attrs)) ] )
# 659 "parser.ml"
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
          MenhirLib.EngineTypes.semv = xs0;
          MenhirLib.EngineTypes.startp = _startpos_xs0_;
          MenhirLib.EngineTypes.endp = _endpos_xs0_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = _1;
            MenhirLib.EngineTypes.startp = _startpos__1_;
            MenhirLib.EngineTypes.endp = _endpos__1_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let xs0 : (Syntax.parameter list) = Obj.magic xs0 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_xs0_ in
        let _v : (Syntax.declaration Positions.located list) = let ss =
          let xs = xs0 in
          
# 158 "parser.mly"
    ( xs )
# 692 "parser.ml"
          
        in
        
# 122 "parser.mly"
    ( let prec = ParserAux.new_on_error_reduce_level() in
      List.map (Positions.map (fun nt -> DOnErrorReduce (nt, prec)))
        (List.map Parameters.with_pos ss) )
# 700 "parser.ml"
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
          MenhirLib.EngineTypes.semv = _10;
          MenhirLib.EngineTypes.startp = _startpos__10_;
          MenhirLib.EngineTypes.endp = _endpos__10_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _10 : unit = Obj.magic _10 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__10_ in
        let _endpos = _endpos__10_ in
        let _v : (Syntax.declaration Positions.located list) = let _1 =
          let _1 = _10 in
          
# 150 "parser.mly"
    ( () )
# 727 "parser.ml"
          
        in
        let _endpos__1_ = _endpos__10_ in
        let _startpos__1_ = _startpos__10_ in
        let _endpos = _endpos__1_ in
        let _startpos = _startpos__1_ in
        
# 131 "parser.mly"
    (
      Error.error (Positions.two _startpos _endpos)
        "syntax error inside a declaration.\n\
         Did you perhaps forget the %%%% that separates declarations and rules?"
    )
# 741 "parser.ml"
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
          MenhirLib.EngineTypes.semv = _10;
          MenhirLib.EngineTypes.startp = _startpos__10_;
          MenhirLib.EngineTypes.endp = _endpos__10_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _10 : unit = Obj.magic _10 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__10_ in
        let _endpos = _endpos__10_ in
        let _v : (Syntax.declaration Positions.located list) = let _1 =
          let _1 = _10 in
          
# 150 "parser.mly"
    ( () )
# 768 "parser.ml"
          
        in
        let _endpos__1_ = _endpos__10_ in
        let _startpos__1_ = _startpos__10_ in
        let _endpos = _endpos__1_ in
        let _startpos = _startpos__1_ in
        
# 131 "parser.mly"
    (
      Error.error (Positions.two _startpos _endpos)
        "syntax error inside a declaration.\n\
         Did you perhaps forget the %%%% that separates declarations and rules?"
    )
# 782 "parser.ml"
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
          MenhirLib.EngineTypes.semv = _10;
          MenhirLib.EngineTypes.startp = _startpos__10_;
          MenhirLib.EngineTypes.endp = _endpos__10_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _10 : unit = Obj.magic _10 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__10_ in
        let _endpos = _endpos__10_ in
        let _v : (Syntax.declaration Positions.located list) = let _1 =
          let _1 = _10 in
          
# 150 "parser.mly"
    ( () )
# 809 "parser.ml"
          
        in
        let _endpos__1_ = _endpos__10_ in
        let _startpos__1_ = _startpos__10_ in
        let _endpos = _endpos__1_ in
        let _startpos = _startpos__1_ in
        
# 131 "parser.mly"
    (
      Error.error (Positions.two _startpos _endpos)
        "syntax error inside a declaration.\n\
         Did you perhaps forget the %%%% that separates declarations and rules?"
    )
# 823 "parser.ml"
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
          MenhirLib.EngineTypes.semv = _10;
          MenhirLib.EngineTypes.startp = _startpos__10_;
          MenhirLib.EngineTypes.endp = _endpos__10_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _10 : unit = Obj.magic _10 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__10_ in
        let _endpos = _endpos__10_ in
        let _v : (Syntax.declaration Positions.located list) = let _1 =
          let _1 = _10 in
          
# 150 "parser.mly"
    ( () )
# 850 "parser.ml"
          
        in
        let _endpos__1_ = _endpos__10_ in
        let _startpos__1_ = _startpos__10_ in
        let _endpos = _endpos__1_ in
        let _startpos = _startpos__1_ in
        
# 131 "parser.mly"
    (
      Error.error (Positions.two _startpos _endpos)
        "syntax error inside a declaration.\n\
         Did you perhaps forget the %%%% that separates declarations and rules?"
    )
# 864 "parser.ml"
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
        let _v : (bool * bool) = 
# 217 "parser.mly"
    ( false, false )
# 882 "parser.ml"
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
        let _v : (bool * bool) = 
# 219 "parser.mly"
    ( true, false )
# 907 "parser.ml"
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
        let _v : (bool * bool) = 
# 221 "parser.mly"
    ( false, true )
# 932 "parser.ml"
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
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (bool * bool) = 
# 224 "parser.mly"
    ( true, true )
# 963 "parser.ml"
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
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : (bool * bool) = 
# 224 "parser.mly"
    ( true, true )
# 994 "parser.ml"
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
          MenhirLib.EngineTypes.semv = t;
          MenhirLib.EngineTypes.startp = _startpos_t_;
          MenhirLib.EngineTypes.endp = _endpos_t_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = rs;
            MenhirLib.EngineTypes.startp = _startpos_rs_;
            MenhirLib.EngineTypes.endp = _endpos_rs_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.semv = _2;
              MenhirLib.EngineTypes.startp = _startpos__2_;
              MenhirLib.EngineTypes.endp = _endpos__2_;
              MenhirLib.EngineTypes.next = {
                MenhirLib.EngineTypes.state = _menhir_s;
                MenhirLib.EngineTypes.semv = ds;
                MenhirLib.EngineTypes.startp = _startpos_ds_;
                MenhirLib.EngineTypes.endp = _endpos_ds_;
                MenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let t : (Syntax.postlude option) = Obj.magic t in
        let rs : (Syntax.parameterized_rule list) = Obj.magic rs in
        let _2 : (
# 39 "parser.mly"
       (Stretch.t Lazy.t)
# 1032 "parser.ml"
        ) = Obj.magic _2 in
        let ds : (Syntax.declaration Positions.located list list) = Obj.magic ds in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_ds_ in
        let _endpos = _endpos_t_ in
        let _v : (
# 49 "parser.mly"
       (Syntax.partial_grammar)
# 1041 "parser.ml"
        ) = 
# 72 "parser.mly"
    (
      {
        pg_filename          = ""; (* filled in by the caller *)
        pg_declarations      = List.flatten ds;
        pg_rules             = rs;
        pg_postlude          = t
      }
    )
# 1052 "parser.ml"
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
          MenhirLib.EngineTypes.semv = params00;
          MenhirLib.EngineTypes.startp = _startpos_params00_;
          MenhirLib.EngineTypes.endp = _endpos_params00_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = symbol0;
            MenhirLib.EngineTypes.startp = _startpos_symbol0_;
            MenhirLib.EngineTypes.endp = _endpos_symbol0_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let params00 : (Syntax.parameters) = Obj.magic params00 in
        let symbol0 : (Syntax.terminal Positions.located) = Obj.magic symbol0 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_symbol0_ in
        let _endpos = _endpos_params00_ in
        let _v : (Syntax.parameter) = let p =
          let params0 = params00 in
          let symbol = symbol0 in
          let actuals =
            let params = params0 in
            
# 352 "parser.mly"
    ( params )
# 1088 "parser.ml"
            
          in
          
# 319 "parser.mly"
    ( Parameters.app symbol actuals )
# 1094 "parser.ml"
          
        in
        
# 334 "parser.mly"
    ( p )
# 1100 "parser.ml"
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
          MenhirLib.EngineTypes.semv = m0;
          MenhirLib.EngineTypes.startp = _startpos_m0_;
          MenhirLib.EngineTypes.endp = _endpos_m0_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = p0;
            MenhirLib.EngineTypes.startp = _startpos_p0_;
            MenhirLib.EngineTypes.endp = _endpos_p0_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let m0 : (Syntax.symbol Positions.located) = Obj.magic m0 in
        let p0 : (Syntax.parameter) = Obj.magic p0 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_p0_ in
        let _endpos = _endpos_m0_ in
        let _v : (Syntax.parameter) = let p =
          let m = m0 in
          let p = p0 in
          
# 322 "parser.mly"
    ( ParameterApp (m, [ p ]) )
# 1134 "parser.ml"
          
        in
        
# 334 "parser.mly"
    ( p )
# 1140 "parser.ml"
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
          MenhirLib.EngineTypes.semv = prods0;
          MenhirLib.EngineTypes.startp = _startpos_prods0_;
          MenhirLib.EngineTypes.endp = _endpos_prods0_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let prods0 : (Syntax.parameterized_branch list list) = Obj.magic prods0 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_prods0_ in
        let _endpos = _endpos_prods0_ in
        let _v : (Syntax.parameter) = let branches =
          let prods = prods0 in
          
# 213 "parser.mly"
    ( List.flatten prods )
# 1167 "parser.ml"
          
        in
        let _endpos_branches_ = _endpos_prods0_ in
        let _startpos_branches_ = _startpos_prods0_ in
        let _endpos = _endpos_branches_ in
        let _startpos = _startpos_branches_ in
        
# 338 "parser.mly"
    ( ParameterAnonymous (with_poss _startpos _endpos branches) )
# 1177 "parser.ml"
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
        let _v : (Syntax.attributes) = 
# 185 "../standard.mly"
    ( [] )
# 1195 "parser.ml"
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
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = x;
            MenhirLib.EngineTypes.startp = _startpos_x_;
            MenhirLib.EngineTypes.endp = _endpos_x_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let xs : (Syntax.attributes) = Obj.magic xs in
        let x : (
# 41 "parser.mly"
       (Syntax.attribute)
# 1222 "parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : (Syntax.attributes) = 
# 187 "../standard.mly"
    ( x :: xs )
# 1230 "parser.ml"
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
        let _v : (Syntax.declaration Positions.located list list) = 
# 185 "../standard.mly"
    ( [] )
# 1248 "parser.ml"
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
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = x;
            MenhirLib.EngineTypes.startp = _startpos_x_;
            MenhirLib.EngineTypes.endp = _endpos_x_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let xs : (Syntax.declaration Positions.located list list) = Obj.magic xs in
        let x : (Syntax.declaration Positions.located list) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : (Syntax.declaration Positions.located list list) = 
# 187 "../standard.mly"
    ( x :: xs )
# 1279 "parser.ml"
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
        let _v : (ParserAux.early_producers) = 
# 185 "../standard.mly"
    ( [] )
# 1297 "parser.ml"
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
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = x;
            MenhirLib.EngineTypes.startp = _startpos_x_;
            MenhirLib.EngineTypes.endp = _endpos_x_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let xs : (ParserAux.early_producers) = Obj.magic xs in
        let x : (
# 47 "parser.mly"
      (ParserAux.early_producer)
# 1324 "parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : (ParserAux.early_producers) = 
# 187 "../standard.mly"
    ( x :: xs )
# 1332 "parser.ml"
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
        let _v : (Syntax.parameterized_rule list) = 
# 185 "../standard.mly"
    ( [] )
# 1350 "parser.ml"
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
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = x;
            MenhirLib.EngineTypes.startp = _startpos_x_;
            MenhirLib.EngineTypes.endp = _endpos_x_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let xs : (Syntax.parameterized_rule list) = Obj.magic xs in
        let x : (Syntax.parameterized_rule) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : (Syntax.parameterized_rule list) = 
# 187 "../standard.mly"
    ( x :: xs )
# 1381 "parser.ml"
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
        let _v : (Syntax.parameters) = 
# 128 "../standard.mly"
    ( [] )
# 1399 "parser.ml"
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
          MenhirLib.EngineTypes.semv = _30;
          MenhirLib.EngineTypes.startp = _startpos__30_;
          MenhirLib.EngineTypes.endp = _endpos__30_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = x0;
            MenhirLib.EngineTypes.startp = _startpos_x0_;
            MenhirLib.EngineTypes.endp = _endpos_x0_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = _10;
              MenhirLib.EngineTypes.startp = _startpos__10_;
              MenhirLib.EngineTypes.endp = _endpos__10_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let _30 : unit = Obj.magic _30 in
        let x0 : (Syntax.parameters) = Obj.magic x0 in
        let _10 : unit = Obj.magic _10 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__10_ in
        let _endpos = _endpos__30_ in
        let _v : (Syntax.parameters) = let x =
          let _3 = _30 in
          let x = x0 in
          let _1 = _10 in
          
# 174 "../standard.mly"
    ( x )
# 1440 "parser.ml"
          
        in
        
# 130 "../standard.mly"
    ( x )
# 1446 "parser.ml"
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
        let _v : (Syntax.parameters) = 
# 128 "../standard.mly"
    ( [] )
# 1464 "parser.ml"
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
          MenhirLib.EngineTypes.semv = _30;
          MenhirLib.EngineTypes.startp = _startpos__30_;
          MenhirLib.EngineTypes.endp = _endpos__30_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = x0;
            MenhirLib.EngineTypes.startp = _startpos_x0_;
            MenhirLib.EngineTypes.endp = _endpos_x0_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = _10;
              MenhirLib.EngineTypes.startp = _startpos__10_;
              MenhirLib.EngineTypes.endp = _endpos__10_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let _30 : unit = Obj.magic _30 in
        let x0 : (Syntax.parameters) = Obj.magic x0 in
        let _10 : unit = Obj.magic _10 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__10_ in
        let _endpos = _endpos__30_ in
        let _v : (Syntax.parameters) = let x =
          let _3 = _30 in
          let x = x0 in
          let _1 = _10 in
          
# 174 "../standard.mly"
    ( x )
# 1505 "parser.ml"
          
        in
        
# 130 "../standard.mly"
    ( x )
# 1511 "parser.ml"
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
        let _v : (Syntax.terminal Positions.located list) = 
# 128 "../standard.mly"
    ( [] )
# 1529 "parser.ml"
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
          MenhirLib.EngineTypes.semv = _30;
          MenhirLib.EngineTypes.startp = _startpos__30_;
          MenhirLib.EngineTypes.endp = _endpos__30_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = x0;
            MenhirLib.EngineTypes.startp = _startpos_x0_;
            MenhirLib.EngineTypes.endp = _endpos_x0_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = _10;
              MenhirLib.EngineTypes.startp = _startpos__10_;
              MenhirLib.EngineTypes.endp = _endpos__10_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let _30 : unit = Obj.magic _30 in
        let x0 : (Syntax.terminal Positions.located list) = Obj.magic x0 in
        let _10 : unit = Obj.magic _10 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos__10_ in
        let _endpos = _endpos__30_ in
        let _v : (Syntax.terminal Positions.located list) = let x =
          let _3 = _30 in
          let x = x0 in
          let _1 = _10 in
          
# 174 "../standard.mly"
    ( x )
# 1570 "parser.ml"
          
        in
        
# 130 "../standard.mly"
    ( x )
# 1576 "parser.ml"
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
        let _v : (Syntax.symbol Positions.located) = let _endpos = _endpos__1_ in
        let _startpos = _startpos__1_ in
        
# 360 "parser.mly"
    ( with_poss _startpos _endpos "option" )
# 1603 "parser.ml"
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
        let _v : (Syntax.symbol Positions.located) = let _endpos = _endpos__1_ in
        let _startpos = _startpos__1_ in
        
# 362 "parser.mly"
    ( with_poss _startpos _endpos "nonempty_list" )
# 1630 "parser.ml"
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
        let _v : (Syntax.symbol Positions.located) = let _endpos = _endpos__1_ in
        let _startpos = _startpos__1_ in
        
# 364 "parser.mly"
    ( with_poss _startpos _endpos "list" )
# 1657 "parser.ml"
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
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : (
# 41 "parser.mly"
       (Syntax.attribute)
# 1678 "parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : (Syntax.attributes) = 
# 195 "../standard.mly"
    ( [ x ] )
# 1686 "parser.ml"
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
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = x;
            MenhirLib.EngineTypes.startp = _startpos_x_;
            MenhirLib.EngineTypes.endp = _endpos_x_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let xs : (Syntax.attributes) = Obj.magic xs in
        let x : (
# 41 "parser.mly"
       (Syntax.attribute)
# 1713 "parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : (Syntax.attributes) = 
# 197 "../standard.mly"
    ( x :: xs )
# 1721 "parser.ml"
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
        let _v : (unit option) = 
# 100 "../standard.mly"
    ( None )
# 1739 "parser.ml"
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
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : unit = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : (unit option) = 
# 102 "../standard.mly"
    ( Some x )
# 1764 "parser.ml"
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
        let _v : (Stretch.ocamltype option) = 
# 100 "../standard.mly"
    ( None )
# 1782 "parser.ml"
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
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : (
# 38 "parser.mly"
       (Stretch.ocamltype)
# 1803 "parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : (Stretch.ocamltype option) = 
# 102 "../standard.mly"
    ( Some x )
# 1811 "parser.ml"
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
        let _v : (unit) = 
# 229 "parser.mly"
    ( () )
# 1829 "parser.ml"
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
        let _v : (unit) = 
# 229 "parser.mly"
    ( () )
# 1854 "parser.ml"
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
        let _v : (Syntax.postlude option) = 
# 371 "parser.mly"
    ( None )
# 1879 "parser.ml"
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
          MenhirLib.EngineTypes.semv = p;
          MenhirLib.EngineTypes.startp = _startpos_p_;
          MenhirLib.EngineTypes.endp = _endpos_p_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let p : (
# 39 "parser.mly"
       (Stretch.t Lazy.t)
# 1900 "parser.ml"
        ) = Obj.magic p in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_p_ in
        let _endpos = _endpos_p_ in
        let _v : (Syntax.postlude option) = 
# 373 "parser.mly"
    ( Some (Lazy.force p) )
# 1908 "parser.ml"
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
        let _v : (Syntax.token_associativity) = 
# 139 "parser.mly"
    ( LeftAssoc )
# 1933 "parser.ml"
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
        let _v : (Syntax.token_associativity) = 
# 141 "parser.mly"
    ( RightAssoc )
# 1958 "parser.ml"
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
        let _v : (Syntax.token_associativity) = 
# 143 "parser.mly"
    ( NonAssoc )
# 1983 "parser.ml"
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
          MenhirLib.EngineTypes.semv = attrs;
          MenhirLib.EngineTypes.startp = _startpos_attrs_;
          MenhirLib.EngineTypes.endp = _endpos_attrs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = p;
            MenhirLib.EngineTypes.startp = _startpos_p_;
            MenhirLib.EngineTypes.endp = _endpos_p_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let attrs : (Syntax.attributes) = Obj.magic attrs in
        let p : (Syntax.parameter) = Obj.magic p in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_p_ in
        let _endpos = _endpos_attrs_ in
        let _v : (
# 47 "parser.mly"
      (ParserAux.early_producer)
# 2014 "parser.ml"
        ) = let id =
          
# 110 "../standard.mly"
    ( None )
# 2019 "parser.ml"
          
        in
        let _startpos_id_ = _endpos__0_ in
        let _endpos = _endpos_attrs_ in
        let _startpos = _startpos_id_ in
        
# 292 "parser.mly"
    ( position (with_poss _startpos _endpos ()), id, p, attrs )
# 2028 "parser.ml"
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
          MenhirLib.EngineTypes.semv = attrs;
          MenhirLib.EngineTypes.startp = _startpos_attrs_;
          MenhirLib.EngineTypes.endp = _endpos_attrs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = p;
            MenhirLib.EngineTypes.startp = _startpos_p_;
            MenhirLib.EngineTypes.endp = _endpos_p_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.semv = _200;
              MenhirLib.EngineTypes.startp = _startpos__200_;
              MenhirLib.EngineTypes.endp = _endpos__200_;
              MenhirLib.EngineTypes.next = {
                MenhirLib.EngineTypes.state = _menhir_s;
                MenhirLib.EngineTypes.semv = x00;
                MenhirLib.EngineTypes.startp = _startpos_x00_;
                MenhirLib.EngineTypes.endp = _endpos_x00_;
                MenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let attrs : (Syntax.attributes) = Obj.magic attrs in
        let p : (Syntax.parameter) = Obj.magic p in
        let _200 : unit = Obj.magic _200 in
        let x00 : (
# 36 "parser.mly"
       (string Positions.located)
# 2067 "parser.ml"
        ) = Obj.magic x00 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x00_ in
        let _endpos = _endpos_attrs_ in
        let _v : (
# 47 "parser.mly"
      (ParserAux.early_producer)
# 2075 "parser.ml"
        ) = let id =
          let _20 = _200 in
          let x0 = x00 in
          let x =
            let _2 = _20 in
            let x = x0 in
            
# 165 "../standard.mly"
    ( x )
# 2085 "parser.ml"
            
          in
          
# 112 "../standard.mly"
    ( Some x )
# 2091 "parser.ml"
          
        in
        let _startpos_id_ = _startpos_x00_ in
        let _endpos = _endpos_attrs_ in
        let _startpos = _startpos_id_ in
        
# 292 "parser.mly"
    ( position (with_poss _startpos _endpos ()), id, p, attrs )
# 2100 "parser.ml"
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
          MenhirLib.EngineTypes.semv = producers;
          MenhirLib.EngineTypes.startp = _startpos_producers_;
          MenhirLib.EngineTypes.endp = _endpos_producers_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let producers : (ParserAux.early_producers) = Obj.magic producers in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_producers_ in
        let _endpos = _endpos_producers_ in
        let _v : (
# 48 "parser.mly"
      (ParserAux.early_production)
# 2125 "parser.ml"
        ) = let oprec =
          
# 110 "../standard.mly"
    ( None )
# 2130 "parser.ml"
          
        in
        let _endpos_oprec_ = _endpos_producers_ in
        let _endpos = _endpos_oprec_ in
        let _startpos = _startpos_producers_ in
        
# 271 "parser.mly"
    ( producers,
      oprec,
      ParserAux.new_production_level(),
      Positions.lex_join _startpos _endpos
    )
# 2143 "parser.ml"
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
          MenhirLib.EngineTypes.semv = symbol00;
          MenhirLib.EngineTypes.startp = _startpos_symbol00_;
          MenhirLib.EngineTypes.endp = _endpos_symbol00_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = _100;
            MenhirLib.EngineTypes.startp = _startpos__100_;
            MenhirLib.EngineTypes.endp = _endpos__100_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = producers;
              MenhirLib.EngineTypes.startp = _startpos_producers_;
              MenhirLib.EngineTypes.endp = _endpos_producers_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let symbol00 : (Syntax.terminal Positions.located) = Obj.magic symbol00 in
        let _100 : unit = Obj.magic _100 in
        let producers : (ParserAux.early_producers) = Obj.magic producers in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_producers_ in
        let _endpos = _endpos_symbol00_ in
        let _v : (
# 48 "parser.mly"
      (ParserAux.early_production)
# 2180 "parser.ml"
        ) = let oprec =
          let symbol0 = symbol00 in
          let _10 = _100 in
          let x =
            let symbol = symbol0 in
            let _1 = _10 in
            
# 263 "parser.mly"
    ( symbol )
# 2190 "parser.ml"
            
          in
          
# 112 "../standard.mly"
    ( Some x )
# 2196 "parser.ml"
          
        in
        let _endpos_oprec_ = _endpos_symbol00_ in
        let _endpos = _endpos_oprec_ in
        let _startpos = _startpos_producers_ in
        
# 271 "parser.mly"
    ( producers,
      oprec,
      ParserAux.new_production_level(),
      Positions.lex_join _startpos _endpos
    )
# 2209 "parser.ml"
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
          MenhirLib.EngineTypes.semv = action;
          MenhirLib.EngineTypes.startp = _startpos_action_;
          MenhirLib.EngineTypes.endp = _endpos_action_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = productions;
            MenhirLib.EngineTypes.startp = _startpos_productions_;
            MenhirLib.EngineTypes.endp = _endpos_productions_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let action : (
# 40 "parser.mly"
       (Syntax.identifier option array -> Action.t)
# 2235 "parser.ml"
        ) = Obj.magic action in
        let productions : (ParserAux.early_productions) = Obj.magic productions in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_productions_ in
        let _endpos = _endpos_action_ in
        let _v : (Syntax.parameterized_branch list) = let oprec2 =
          
# 110 "../standard.mly"
    ( None )
# 2245 "parser.ml"
          
        in
        
# 239 "parser.mly"
    (
      (* If multiple productions share a single semantic action, check
         that all of them bind the same names. *)
      ParserAux.check_production_group productions;
      (* Then, *)
      List.map (fun (producers, oprec1, level, pos) ->
        (* Replace [$i] with [_i]. *)
        let pr_producers = ParserAux.normalize_producers producers in
        (* Distribute the semantic action. Also, check that every [$i]
           is within bounds. *)
        let action : Syntax.identifier option array -> Action.t = action in
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
# 2271 "parser.ml"
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
          MenhirLib.EngineTypes.semv = symbol00;
          MenhirLib.EngineTypes.startp = _startpos_symbol00_;
          MenhirLib.EngineTypes.endp = _endpos_symbol00_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = _100;
            MenhirLib.EngineTypes.startp = _startpos__100_;
            MenhirLib.EngineTypes.endp = _endpos__100_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.semv = action;
              MenhirLib.EngineTypes.startp = _startpos_action_;
              MenhirLib.EngineTypes.endp = _endpos_action_;
              MenhirLib.EngineTypes.next = {
                MenhirLib.EngineTypes.state = _menhir_s;
                MenhirLib.EngineTypes.semv = productions;
                MenhirLib.EngineTypes.startp = _startpos_productions_;
                MenhirLib.EngineTypes.endp = _endpos_productions_;
                MenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let symbol00 : (Syntax.terminal Positions.located) = Obj.magic symbol00 in
        let _100 : unit = Obj.magic _100 in
        let action : (
# 40 "parser.mly"
       (Syntax.identifier option array -> Action.t)
# 2309 "parser.ml"
        ) = Obj.magic action in
        let productions : (ParserAux.early_productions) = Obj.magic productions in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_productions_ in
        let _endpos = _endpos_symbol00_ in
        let _v : (Syntax.parameterized_branch list) = let oprec2 =
          let symbol0 = symbol00 in
          let _10 = _100 in
          let x =
            let symbol = symbol0 in
            let _1 = _10 in
            
# 263 "parser.mly"
    ( symbol )
# 2324 "parser.ml"
            
          in
          
# 112 "../standard.mly"
    ( Some x )
# 2330 "parser.ml"
          
        in
        
# 239 "parser.mly"
    (
      (* If multiple productions share a single semantic action, check
         that all of them bind the same names. *)
      ParserAux.check_production_group productions;
      (* Then, *)
      List.map (fun (producers, oprec1, level, pos) ->
        (* Replace [$i] with [_i]. *)
        let pr_producers = ParserAux.normalize_producers producers in
        (* Distribute the semantic action. Also, check that every [$i]
           is within bounds. *)
        let action : Syntax.identifier option array -> Action.t = action in
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
# 2356 "parser.ml"
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
          MenhirLib.EngineTypes.semv = prods0;
          MenhirLib.EngineTypes.startp = _startpos_prods0_;
          MenhirLib.EngineTypes.endp = _endpos_prods0_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = _6;
            MenhirLib.EngineTypes.startp = _startpos__6_;
            MenhirLib.EngineTypes.endp = _endpos__6_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.semv = _5;
              MenhirLib.EngineTypes.startp = _startpos__5_;
              MenhirLib.EngineTypes.endp = _endpos__5_;
              MenhirLib.EngineTypes.next = {
                MenhirLib.EngineTypes.semv = params0;
                MenhirLib.EngineTypes.startp = _startpos_params0_;
                MenhirLib.EngineTypes.endp = _endpos_params0_;
                MenhirLib.EngineTypes.next = {
                  MenhirLib.EngineTypes.semv = attributes;
                  MenhirLib.EngineTypes.startp = _startpos_attributes_;
                  MenhirLib.EngineTypes.endp = _endpos_attributes_;
                  MenhirLib.EngineTypes.next = {
                    MenhirLib.EngineTypes.semv = symbol;
                    MenhirLib.EngineTypes.startp = _startpos_symbol_;
                    MenhirLib.EngineTypes.endp = _endpos_symbol_;
                    MenhirLib.EngineTypes.next = {
                      MenhirLib.EngineTypes.state = _menhir_s;
                      MenhirLib.EngineTypes.semv = flags;
                      MenhirLib.EngineTypes.startp = _startpos_flags_;
                      MenhirLib.EngineTypes.endp = _endpos_flags_;
                      MenhirLib.EngineTypes.next = _menhir_stack;
                    };
                  };
                };
              };
            };
          };
        } = _menhir_stack in
        let prods0 : (Syntax.parameterized_branch list list) = Obj.magic prods0 in
        let _6 : (unit) = Obj.magic _6 in
        let _5 : unit = Obj.magic _5 in
        let params0 : (Syntax.terminal Positions.located list) = Obj.magic params0 in
        let attributes : (Syntax.attributes) = Obj.magic attributes in
        let symbol : (Syntax.terminal Positions.located) = Obj.magic symbol in
        let flags : (bool * bool) = Obj.magic flags in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_flags_ in
        let _endpos = _endpos_prods0_ in
        let _v : (Syntax.parameterized_rule) = let branches =
          let prods = prods0 in
          
# 213 "parser.mly"
    ( List.flatten prods )
# 2419 "parser.ml"
          
        in
        let params =
          let params = params0 in
          
# 352 "parser.mly"
    ( params )
# 2427 "parser.ml"
          
        in
        
# 198 "parser.mly"
    (
      let public, inline = flags in
      {
        pr_public_flag = public;
        pr_inline_flag = inline;
        pr_nt          = Positions.value symbol;
        pr_positions   = [ Positions.position symbol ];
        pr_attributes  = attributes;
        pr_parameters  = List.map Positions.value params;
        pr_branches    = branches
      }
    )
# 2444 "parser.ml"
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
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : (
# 48 "parser.mly"
      (ParserAux.early_production)
# 2465 "parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : (ParserAux.early_productions) = 
# 215 "../standard.mly"
    ( [ x ] )
# 2473 "parser.ml"
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
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = _2;
            MenhirLib.EngineTypes.startp = _startpos__2_;
            MenhirLib.EngineTypes.endp = _endpos__2_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = x;
              MenhirLib.EngineTypes.startp = _startpos_x_;
              MenhirLib.EngineTypes.endp = _endpos_x_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : (ParserAux.early_productions) = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : (
# 48 "parser.mly"
      (ParserAux.early_production)
# 2506 "parser.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : (ParserAux.early_productions) = 
# 217 "../standard.mly"
    ( x :: xs )
# 2514 "parser.ml"
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
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : (Syntax.parameterized_branch list) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : (Syntax.parameterized_branch list list) = 
# 215 "../standard.mly"
    ( [ x ] )
# 2539 "parser.ml"
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
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = _2;
            MenhirLib.EngineTypes.startp = _startpos__2_;
            MenhirLib.EngineTypes.endp = _endpos__2_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = x;
              MenhirLib.EngineTypes.startp = _startpos_x_;
              MenhirLib.EngineTypes.endp = _endpos_x_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : (Syntax.parameterized_branch list list) = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : (Syntax.parameterized_branch list) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : (Syntax.parameterized_branch list list) = 
# 217 "../standard.mly"
    ( x :: xs )
# 2576 "parser.ml"
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
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : (Syntax.parameter) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : (Syntax.parameters) = 
# 215 "../standard.mly"
    ( [ x ] )
# 2601 "parser.ml"
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
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = _2;
            MenhirLib.EngineTypes.startp = _startpos__2_;
            MenhirLib.EngineTypes.endp = _endpos__2_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = x;
              MenhirLib.EngineTypes.startp = _startpos_x_;
              MenhirLib.EngineTypes.endp = _endpos_x_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : (Syntax.parameters) = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : (Syntax.parameter) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : (Syntax.parameters) = 
# 217 "../standard.mly"
    ( x :: xs )
# 2638 "parser.ml"
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
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : (Syntax.parameter) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : (Syntax.parameters) = 
# 215 "../standard.mly"
    ( [ x ] )
# 2663 "parser.ml"
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
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = _2;
            MenhirLib.EngineTypes.startp = _startpos__2_;
            MenhirLib.EngineTypes.endp = _endpos__2_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = x;
              MenhirLib.EngineTypes.startp = _startpos_x_;
              MenhirLib.EngineTypes.endp = _endpos_x_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : (Syntax.parameters) = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : (Syntax.parameter) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : (Syntax.parameters) = 
# 217 "../standard.mly"
    ( x :: xs )
# 2700 "parser.ml"
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
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : (Syntax.terminal Positions.located) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : (Syntax.terminal Positions.located list) = 
# 215 "../standard.mly"
    ( [ x ] )
# 2725 "parser.ml"
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
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = _2;
            MenhirLib.EngineTypes.startp = _startpos__2_;
            MenhirLib.EngineTypes.endp = _endpos__2_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = x;
              MenhirLib.EngineTypes.startp = _startpos_x_;
              MenhirLib.EngineTypes.endp = _endpos_x_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : (Syntax.terminal Positions.located list) = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : (Syntax.terminal Positions.located) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : (Syntax.terminal Positions.located list) = 
# 217 "../standard.mly"
    ( x :: xs )
# 2762 "parser.ml"
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
          MenhirLib.EngineTypes.semv = id0;
          MenhirLib.EngineTypes.startp = _startpos_id0_;
          MenhirLib.EngineTypes.endp = _endpos_id0_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let id0 : (
# 36 "parser.mly"
       (string Positions.located)
# 2783 "parser.ml"
        ) = Obj.magic id0 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_id0_ in
        let _endpos = _endpos_id0_ in
        let _v : (Syntax.nonterminal Positions.located list) = let x =
          let id = id0 in
          
# 183 "parser.mly"
    ( id )
# 2793 "parser.ml"
          
        in
        
# 215 "../standard.mly"
    ( [ x ] )
# 2799 "parser.ml"
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
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = _2;
            MenhirLib.EngineTypes.startp = _startpos__2_;
            MenhirLib.EngineTypes.endp = _endpos__2_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = id0;
              MenhirLib.EngineTypes.startp = _startpos_id0_;
              MenhirLib.EngineTypes.endp = _endpos_id0_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : (Syntax.nonterminal Positions.located list) = Obj.magic xs in
        let _2 : (unit option) = Obj.magic _2 in
        let id0 : (
# 36 "parser.mly"
       (string Positions.located)
# 2832 "parser.ml"
        ) = Obj.magic id0 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_id0_ in
        let _endpos = _endpos_xs_ in
        let _v : (Syntax.nonterminal Positions.located list) = let x =
          let id = id0 in
          
# 183 "parser.mly"
    ( id )
# 2842 "parser.ml"
          
        in
        
# 217 "../standard.mly"
    ( x :: xs )
# 2848 "parser.ml"
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
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : (Syntax.parameter) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : (Syntax.parameter list) = 
# 215 "../standard.mly"
    ( [ x ] )
# 2873 "parser.ml"
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
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = _2;
            MenhirLib.EngineTypes.startp = _startpos__2_;
            MenhirLib.EngineTypes.endp = _endpos__2_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = x;
              MenhirLib.EngineTypes.startp = _startpos_x_;
              MenhirLib.EngineTypes.endp = _endpos_x_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : (Syntax.parameter list) = Obj.magic xs in
        let _2 : (unit option) = Obj.magic _2 in
        let x : (Syntax.parameter) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : (Syntax.parameter list) = 
# 217 "../standard.mly"
    ( x :: xs )
# 2910 "parser.ml"
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
          MenhirLib.EngineTypes.semv = x;
          MenhirLib.EngineTypes.startp = _startpos_x_;
          MenhirLib.EngineTypes.endp = _endpos_x_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : (Syntax.terminal Positions.located) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : (Syntax.terminal Positions.located list) = 
# 215 "../standard.mly"
    ( [ x ] )
# 2935 "parser.ml"
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
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = _2;
            MenhirLib.EngineTypes.startp = _startpos__2_;
            MenhirLib.EngineTypes.endp = _endpos__2_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.state = _menhir_s;
              MenhirLib.EngineTypes.semv = x;
              MenhirLib.EngineTypes.startp = _startpos_x_;
              MenhirLib.EngineTypes.endp = _endpos_x_;
              MenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : (Syntax.terminal Positions.located list) = Obj.magic xs in
        let _2 : (unit option) = Obj.magic _2 in
        let x : (Syntax.terminal Positions.located) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : (Syntax.terminal Positions.located list) = 
# 217 "../standard.mly"
    ( x :: xs )
# 2972 "parser.ml"
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
          MenhirLib.EngineTypes.semv = attrs0;
          MenhirLib.EngineTypes.startp = _startpos_attrs0_;
          MenhirLib.EngineTypes.endp = _endpos_attrs0_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = id0;
            MenhirLib.EngineTypes.startp = _startpos_id0_;
            MenhirLib.EngineTypes.endp = _endpos_id0_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let attrs0 : (Syntax.attributes) = Obj.magic attrs0 in
        let id0 : (
# 36 "parser.mly"
       (string Positions.located)
# 2999 "parser.ml"
        ) = Obj.magic id0 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_id0_ in
        let _endpos = _endpos_attrs0_ in
        let _v : ((Syntax.terminal * Syntax.attributes) Positions.located list) = let x =
          let attrs = attrs0 in
          let id = id0 in
          
# 179 "parser.mly"
    ( Positions.map (fun uid -> (uid, attrs)) id )
# 3010 "parser.ml"
          
        in
        
# 215 "../standard.mly"
    ( [ x ] )
# 3016 "parser.ml"
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
          MenhirLib.EngineTypes.semv = xs;
          MenhirLib.EngineTypes.startp = _startpos_xs_;
          MenhirLib.EngineTypes.endp = _endpos_xs_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.semv = _2;
            MenhirLib.EngineTypes.startp = _startpos__2_;
            MenhirLib.EngineTypes.endp = _endpos__2_;
            MenhirLib.EngineTypes.next = {
              MenhirLib.EngineTypes.semv = attrs0;
              MenhirLib.EngineTypes.startp = _startpos_attrs0_;
              MenhirLib.EngineTypes.endp = _endpos_attrs0_;
              MenhirLib.EngineTypes.next = {
                MenhirLib.EngineTypes.state = _menhir_s;
                MenhirLib.EngineTypes.semv = id0;
                MenhirLib.EngineTypes.startp = _startpos_id0_;
                MenhirLib.EngineTypes.endp = _endpos_id0_;
                MenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let xs : ((Syntax.terminal * Syntax.attributes) Positions.located list) = Obj.magic xs in
        let _2 : (unit option) = Obj.magic _2 in
        let attrs0 : (Syntax.attributes) = Obj.magic attrs0 in
        let id0 : (
# 36 "parser.mly"
       (string Positions.located)
# 3055 "parser.ml"
        ) = Obj.magic id0 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_id0_ in
        let _endpos = _endpos_xs_ in
        let _v : ((Syntax.terminal * Syntax.attributes) Positions.located list) = let x =
          let attrs = attrs0 in
          let id = id0 in
          
# 179 "parser.mly"
    ( Positions.map (fun uid -> (uid, attrs)) id )
# 3066 "parser.ml"
          
        in
        
# 217 "../standard.mly"
    ( x :: xs )
# 3072 "parser.ml"
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
          MenhirLib.EngineTypes.semv = params00;
          MenhirLib.EngineTypes.startp = _startpos_params00_;
          MenhirLib.EngineTypes.endp = _endpos_params00_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = symbol0;
            MenhirLib.EngineTypes.startp = _startpos_symbol0_;
            MenhirLib.EngineTypes.endp = _endpos_symbol0_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let params00 : (Syntax.parameters) = Obj.magic params00 in
        let symbol0 : (Syntax.terminal Positions.located) = Obj.magic symbol0 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_symbol0_ in
        let _endpos = _endpos_params00_ in
        let _v : (Syntax.parameter) = let p =
          let params0 = params00 in
          let symbol = symbol0 in
          let actuals =
            let params = params0 in
            
# 352 "parser.mly"
    ( params )
# 3108 "parser.ml"
            
          in
          
# 319 "parser.mly"
    ( Parameters.app symbol actuals )
# 3114 "parser.ml"
          
        in
        
# 326 "parser.mly"
    ( p )
# 3120 "parser.ml"
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
          MenhirLib.EngineTypes.semv = m0;
          MenhirLib.EngineTypes.startp = _startpos_m0_;
          MenhirLib.EngineTypes.endp = _endpos_m0_;
          MenhirLib.EngineTypes.next = {
            MenhirLib.EngineTypes.state = _menhir_s;
            MenhirLib.EngineTypes.semv = p0;
            MenhirLib.EngineTypes.startp = _startpos_p0_;
            MenhirLib.EngineTypes.endp = _endpos_p0_;
            MenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let m0 : (Syntax.symbol Positions.located) = Obj.magic m0 in
        let p0 : (Syntax.parameter) = Obj.magic p0 in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_p0_ in
        let _endpos = _endpos_m0_ in
        let _v : (Syntax.parameter) = let p =
          let m = m0 in
          let p = p0 in
          
# 322 "parser.mly"
    ( ParameterApp (m, [ p ]) )
# 3154 "parser.ml"
          
        in
        
# 326 "parser.mly"
    ( p )
# 3160 "parser.ml"
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
          MenhirLib.EngineTypes.semv = id;
          MenhirLib.EngineTypes.startp = _startpos_id_;
          MenhirLib.EngineTypes.endp = _endpos_id_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let id : (
# 36 "parser.mly"
       (string Positions.located)
# 3181 "parser.ml"
        ) = Obj.magic id in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_id_ in
        let _endpos = _endpos_id_ in
        let _v : (Syntax.terminal Positions.located) = 
# 171 "parser.mly"
    ( id )
# 3189 "parser.ml"
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
          MenhirLib.EngineTypes.semv = id;
          MenhirLib.EngineTypes.startp = _startpos_id_;
          MenhirLib.EngineTypes.endp = _endpos_id_;
          MenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let id : (
# 36 "parser.mly"
       (string Positions.located)
# 3210 "parser.ml"
        ) = Obj.magic id in
        let _endpos__0_ = _menhir_stack.MenhirLib.EngineTypes.endp in
        let _startpos = _startpos_id_ in
        let _endpos = _endpos_id_ in
        let _v : (Syntax.terminal Positions.located) = 
# 171 "parser.mly"
    ( id )
# 3218 "parser.ml"
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

let grammar =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 0 lexer lexbuf) : (
# 49 "parser.mly"
       (Syntax.partial_grammar)
# 3249 "parser.ml"
    ))

module Incremental = struct
  
  let grammar =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 0 initial_position) : (
# 49 "parser.mly"
       (Syntax.partial_grammar)
# 3259 "parser.ml"
      ) MenhirInterpreter.checkpoint)
  
end

# 375 "parser.mly"
  

# 3267 "parser.ml"

# 219 "../standard.mly"
  


# 3273 "parser.ml"
