exception SyntaxError of string
type pos = Lexing.position
and symbol = Symbol.symbol
and ty =
    IntTy
  | BoolTy
  | CharTy
  | StringTy
  | ArrayTy of ty
  | PairTy of ty * ty
  | PairTyy
  | NullTy
and binop =
    PlusOp
  | MinusOp
  | TimesOp
  | DivideOp
  | GeOp
  | GtOp
  | EqOp
  | NeOp
  | LtOp
  | LeOp
  | AndOp
  | OrOp
  | ModOp
and unop = NotOp | NegOp | LenOp | OrdOp | ChrOp
(* exp are expressions that you would normally expect from the LHS of stmt *)
and exp =
  | IdentExp      of symbol * pos
  | ArrayIndexExp of symbol * exp list * pos
  | LiteralExp  of literal * pos
  | BinOpExp    of exp * binop * exp * pos
  | UnOpExp     of unop * exp * pos
  | NullExp     of pos
  | NewPairExp  of exp * exp * pos
  | CallExp     of symbol * (exp list) * pos            (* call a function *)
  | FstExp      of exp * pos
  | SndExp      of exp * pos
(* stmt differentiate from exp for having side effects *)
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
    LitString of string
  | LitBool of bool
  | LitChar of char
  | LitInt of int
  | LitArray of exp list
  | LitPair of (exp * exp)
  | Null
(* for a particular field in function definition *)
and field = ty * symbol
and dec = FuncDec of ty * symbol * field list * stmt * pos
and t = dec list * stmt
val check_int_overflow : int -> int
val unop_types : (unop * ty * ty) list
val unop_arg_type : unop -> ty
val unop_ret_type : unop -> ty
val pp_type : ty -> string
val pp_literal : literal -> string
val pp_binop : binop -> string
val pp_unop : unop -> string
val pp_exp : exp -> string
val pp_stmt : stmt -> string
