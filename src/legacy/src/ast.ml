exception SyntaxError of string

type pos = Lexing.position
and symbol = Symbol.symbol
and ty =
  | IntTy
  | BoolTy
  | CharTy
  | StringTy
  | ArrayTy of ty
  | PairTy of ty * ty
  | PairTyy
  | NullTy
and binop =
  | PlusOp                        (* + *)
  | MinusOp                       (* - *)
  | TimesOp                       (* * *)
  | DivideOp                      (* / *)
  | GeOp                          (* >= *)
  | GtOp                          (* > *)
  | EqOp                          (* == *)
  | NeOp                          (* != *)
  | LtOp                          (* < *)
  | LeOp                          (* <= *)
  | AndOp                         (* && *)
  | OrOp                          (* || *)
  | ModOp                         (* mod *)
and unop =
  | NotOp                         (* ! *)
  | NegOp                         (* - *)
  | LenOp                         (* len *)
  | OrdOp                         (* ord *)
  | ChrOp                         (* chr *)
and exp =
  | IdentExp      of symbol * pos
  | ArrayIndexExp of symbol * exp list * pos
  | LiteralExp    of literal * pos
  | BinOpExp      of exp * binop * exp * pos
  | UnOpExp       of unop * exp * pos
  | NullExp       of pos
  | NewPairExp    of exp * exp * pos
  | CallExp       of symbol * (exp list) * pos            (* call a function *)
  | FstExp        of exp * pos
  | SndExp        of exp * pos
and stmt =
  | SkipStmt of pos
  | VarDeclStmt of ty * symbol * exp * pos
  | AssignStmt of exp * exp * pos
  | ReadStmt of exp * pos
  | FreeStmt of exp * pos
  | ExitStmt of exp * pos
  | PrintStmt of exp * pos
  | PrintLnStmt of exp * pos
  | IfStmt of exp * stmt * stmt * pos
  | WhileStmt of exp * stmt * pos
  | BlockStmt of stmt * pos
  | RetStmt of exp * pos
  | SeqStmt of stmt list
and literal =
  | LitString of string
  | LitBool of bool
  | LitChar of char
  | LitInt of int
  | LitArray of (exp list)
  | LitPair of (exp * exp)
  | Null
and field = ty * symbol
and dec = FuncDec of ty * symbol * (field list) * stmt * pos

and t = dec list * stmt

let var_name = function
  | IdentExp (name, _) -> name
  | ArrayIndexExp (name, _, _) -> name
  | _ -> raise (Invalid_argument "not a var")

let check_int_overflow num =
  let max_int = Int32.to_int Int32.max_int in
  let min_int = Int32.to_int Int32.min_int in
  if num <= max_int && num >= min_int
  then num
  else raise (SyntaxError ("Int overflow: " ^ string_of_int num));;


(* UnOp ArgType ReturnType *)
let unop_types = [
  (NotOp, BoolTy, BoolTy);
  (NegOp, IntTy, IntTy);
  (LenOp, ArrayTy NullTy, IntTy);
  (OrdOp, CharTy, IntTy);
  (ChrOp, IntTy, CharTy)
]
let unop_arg_type = function
  | NotOp -> BoolTy
  | NegOp -> IntTy
  | LenOp -> ArrayTy NullTy
  | OrdOp -> CharTy
  | ChrOp -> IntTy

let unop_ret_type = function
  | NotOp -> BoolTy
  | NegOp -> IntTy
  | LenOp -> IntTy
  | OrdOp -> IntTy
  | ChrOp -> CharTy

let rec pp_type = function
  | BoolTy -> "bool"
  | ArrayTy t -> pp_type t ^ "[]"
  | PairTyy -> "pair"
  | IntTy -> "int"
  | PairTy (t1, t2) -> "pair(" ^ pp_type t1 ^ ", " ^ pp_type t2 ^ ")"
  | CharTy -> "char"
  | StringTy -> "string"
  | NullTy -> "pair"
and pp_literal = function
  | LitString str -> "\"" ^ str ^ "\""
  | LitBool b -> string_of_bool b
  | LitChar c -> "\'" ^ (Char.escaped c) ^ "\'"
  | LitInt i  -> string_of_int i
  | LitArray exps -> "[" ^ String.concat ", " (List.map pp_exp exps) ^ "]"
  | LitPair (fst_exp, snd_exp) -> "(" ^ pp_exp fst_exp ^ ", " ^ pp_exp snd_exp ^ ")"
  | Null -> "null"
(* pretty print expressions *)
and pp_binop = function
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
and pp_unop = function
  | NotOp -> "!"                         (* ! *)
  | NegOp -> "-"                         (* - *)
  | LenOp -> "len"                       (* len *)
  | OrdOp -> "ord"                       (* ord *)
  | ChrOp -> "chr"                       (* chr *)
and pp_exp = function
  | IdentExp    (name, pos) -> Symbol.name name
  | LiteralExp  (literal, pos) -> pp_literal literal
  | BinOpExp    (exp, binop, exp', pos) -> pp_exp exp ^ pp_binop binop ^ pp_exp exp'
  | UnOpExp     (unop, exp, pos) -> pp_unop unop ^ pp_exp exp
  | NullExp     (pos)  -> "null"
  | NewPairExp  (exp, exp', pos) -> "newpair(" ^ pp_exp exp ^ ", " ^ pp_exp exp' ^ ")"
  | CallExp     (fname, exps, pos) -> "call " ^ Symbol.name fname ^ "(" ^ String.concat ", " (List.map pp_exp exps) ^ ")"
  | ArrayIndexExp (name, exps, pos) -> Symbol.name name
  | FstExp      (exp, pos) -> "fst " ^ pp_exp exp
  | SndExp      (exp, pos) -> "snd " ^ pp_exp exp
and pp_stmt = function
  | AssignStmt   (lhs, rhs, pos) -> pp_exp lhs ^ "=" ^ pp_exp rhs
  | IfStmt       (pred, then_, else_, pos) -> "if (" ^ pp_exp  pred ^ ")" ^ "then " ^ pp_stmt then_ ^ "else " ^ pp_stmt else_ 
  | WhileStmt    (pred, body, pos) -> "while" ^ pp_exp pred ^ "{" ^ pp_stmt body ^ "}"
  | ExitStmt     (exp, pos) ->  "exit " ^ pp_exp exp
  | VarDeclStmt  (ty, name, exp, pos) -> pp_type ty ^ " " ^ Symbol.name name ^ " = " ^ pp_exp exp
  | PrintStmt    (exp, pos) ->  "print " ^ pp_exp exp
  | PrintLnStmt  (exp, pos) ->  "println "^ pp_exp exp
  | RetStmt      (exp, pos) ->  "return" ^ pp_exp exp
  | SeqStmt      (exps) -> String.concat ";\n" (List.map pp_stmt exps)
  | ReadStmt     (exp, pos) -> "read " ^ pp_exp exp
  | FreeStmt     (exp, pos) -> "free " ^ pp_exp exp
  | SkipStmt     (pos) -> "skip"
  | BlockStmt    (block, pos) -> "begin " ^ pp_stmt block ^ " end"
