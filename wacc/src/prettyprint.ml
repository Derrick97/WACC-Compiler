open Ast;;
let rec prettyprint_type = function
  | BoolTy -> "bool"
  | ArrayTy t -> prettyprint_type t ^ "[]"
  | PairTyy -> "pair"
  | IntTy -> "int"
  | PairTy (t1, t2) -> "pair(" ^ prettyprint_type t1 ^ ", " ^ prettyprint_type t2 ^ ")"
  | CharTy -> "char"
  | StringTy -> "string"
  | NullTy -> "pair"
and prettyprint_literal = function
  | LitString str -> "\"" ^ str ^ "\""
  | LitBool b -> string_of_bool b
  | LitChar c -> "\'" ^ (Char.escaped c) ^ "\'"
  | LitInt i  -> string_of_int i
  | LitArray exps -> "[" ^ String.concat ", " (List.map prettyprint_exp exps) ^ "]"
  | LitPair (fst_exp, snd_exp) -> "(" ^ prettyprint_exp fst_exp ^ ", " ^ prettyprint_exp snd_exp ^ ")"
  | Null -> "null"
(* pretty print expressions *)
and prettyprint_binop = function
  | PlusOp   -> " + "                    (* + *)
  | MinusOp  -> " - "                    (* - *)
  | TimesOp  -> " * "                    (* * *)
  | DivideOp -> " / "                    (* / *)
  | GeOp     -> " >= "                   (* >= *)
  | GtOp     -> " > "                    (* > *)
  | EqOp     -> " == "                   (* == *)
  | NeOp     -> " != "                   (* != *)
  | LtOp     -> " < "                    (* < *)
  | LeOp     -> " <= "                   (* <= *)
  | AndOp    -> " && "                   (* && *)
  | OrOp     -> " || "                   (* || *)
  | ModOp    -> " mod "                  (* mod *)
and prettyprint_unop = function
  | NotOp -> "!"                         (* ! *)
  | NegOp -> "-"                         (* - *)
  | LenOp -> "len"                       (* len *)
  | OrdOp -> "ord"                       (* ord *)
  | ChrOp -> "chr"                       (* chr *)
and prettyprint_exp = function
  | IdentExp    (name, pos) -> name
  | LiteralExp  (literal, pos) -> prettyprint_literal literal
  | BinOpExp    (exp, binop, exp', pos) -> prettyprint_exp exp ^ prettyprint_binop binop ^ prettyprint_exp exp'
  | UnOpExp     (unop, exp, pos) -> prettyprint_unop unop ^ prettyprint_exp exp
  | NullExp     (pos)  -> "null"
  | NewPairExp  (exp, exp', pos) -> "newpair(" ^ prettyprint_exp exp ^ ", " ^ prettyprint_exp exp' ^ ")"
  | CallExp     (fname, exps, pos) -> "call " ^ fname ^ "(" ^ String.concat ", " (List.map prettyprint_exp exps) ^ ")"
  | ArrayIndexExp (name, exps, pos) -> name
  | FstExp      (exp, pos) -> "fst " ^ prettyprint_exp exp
  | SndExp      (exp, pos) -> "snd " ^ prettyprint_exp exp
and prettyprint_stmt = function
  | AssignStmt   (lhs, rhs, pos) -> prettyprint_exp lhs ^ "=" ^ prettyprint_exp rhs
  | IfStmt       (pred, then_, else_, pos) -> "if (" ^ prettyprint_exp  pred ^ ")" ^ "then " ^ prettyprint_stmt then_ ^ "else " ^ prettyprint_stmt else_
  | WhileStmt    (pred, body, pos) -> "while" ^ prettyprint_exp pred ^ "{" ^ prettyprint_stmt body ^ "}"
  | ExitStmt     (exp, pos) ->  "exit " ^ prettyprint_exp exp
  | VarDeclStmt  (ty, name, exp, pos) -> prettyprint_type ty ^ " " ^ name ^ " = " ^ prettyprint_exp exp
  | PrintStmt    (newline, exp, pos) ->  (if newline
    then
      "println "^ prettyprint_exp exp
    else "print " ^ prettyprint_exp exp)
  | RetStmt      (exp, pos) ->  "return" ^ prettyprint_exp exp
  | SeqStmt      (stmt, stmtlist) -> prettyprint_stmt stmt ^ ";\n" ^ (prettyprint_stmt stmtlist)
  | ReadStmt     (exp, pos) -> "read " ^ prettyprint_exp exp
  | FreeStmt     (exp, pos) -> "free " ^ prettyprint_exp exp
  | SkipStmt     (pos) -> "skip"
  | BlockStmt    (block, pos) -> "begin " ^ prettyprint_stmt block ^ " end"
