open Ast_v2;;

let rec prettyprint_type = function
  | BoolTy -> "bool"
  | ArrayTy t -> prettyprint_type t ^ "[]"
  | PairTyy -> "pair"
  | IntTy -> "int"
  | PairTy (t1, t2) -> "pair(" ^ prettyprint_type t1 ^ ", " ^ prettyprint_type t2 ^ ")"
  | CharTy -> "char"
  | StringTy -> "string"
  | NullTy -> "pair"
and prettyprint_literal l =
  match l with
  | LitString str -> "\"" ^ str ^ "\""
  | LitBool b -> string_of_bool b
  | LitChar c -> "\'" ^ (Char.escaped c) ^ "\'"
  | LitInt i  -> string_of_int i
  | LitArray exps -> "[" ^ String.concat ", " (List.map pp_exp exps) ^ "]"
  | LitPair (fst_exp, snd_exp) -> "(" ^ pp_exp fst_exp ^ ", " ^ pp_exp snd_exp ^ ")"
  | LitNull -> "null"
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
and pp_exp (exp, _) = prettyprint_exp exp
and prettyprint_exp (e: exp') =
  match e with
  | IdentExp       name -> name
  | LiteralExp     literal -> prettyprint_literal literal
  | BinOpExp      (exp, binop, exp') -> pp_exp exp ^ prettyprint_binop binop ^ pp_exp exp'
  | UnOpExp       (unop, exp) -> prettyprint_unop unop ^ pp_exp exp
  | NullExp        -> "null"
  | NewPairExp    (exp, exp') -> "newpair(" ^ pp_exp exp ^ ", " ^ pp_exp exp' ^ ")"
  | CallExp       (fname, exps) -> "call " ^ fname ^ "(" ^ String.concat ", " (List.map pp_exp exps) ^ ")"
  | ArrayIndexExp (name, exps) -> name
  | FstExp         exp -> "fst " ^ pp_exp exp
  | SndExp         exp -> "snd " ^ pp_exp exp
and pp_stmt (stmt, _) = prettyprint_stmt stmt
and prettyprint_stmt stmt: string =
  match stmt with
  | AssignStmt    (lhs, rhs) -> pp_exp lhs ^ "=" ^ pp_exp rhs
  | IfStmt        (pred, then_, else_) -> "if (" ^ pp_exp  pred ^ ")" ^ "then " ^ pp_stmt then_ ^ "else " ^ pp_stmt else_
  | WhileStmt     (pred, body) -> "while" ^ pp_exp pred ^ "{" ^ pp_stmt body ^ "}"
  | ExitStmt      (exp) ->  "exit " ^ pp_exp exp
  | VarDeclStmt   (ty, name, exp) -> prettyprint_type ty ^ " " ^ name ^ " = " ^ pp_exp exp
  | PrintStmt     (newline, exp) ->  (if newline then "println "^ pp_exp exp
                                          else "print " ^ pp_exp exp)
  | RetStmt      (exp) ->  "return" ^ pp_exp exp
  | SeqStmt      (stmt, stmtlist) -> pp_stmt stmt ^ ";\n" ^ (pp_stmt stmtlist)
  | ReadStmt     exp -> "read " ^ pp_exp exp
  | FreeStmt     exp -> "free " ^ pp_exp exp
  | SkipStmt     -> "skip"
  | BlockStmt   block -> "begin " ^ pp_stmt block ^ " end"
