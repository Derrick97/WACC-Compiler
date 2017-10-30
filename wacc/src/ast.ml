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
  | NullExp     (*of pos*)
  | NewPairExp  of exp * exp (* pos*)
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
  | WhileStmt of exp * stmt (* pos*)
  | BlockStmt of stmt * pos
  | RetStmt of exp * pos
  | SeqStmt of stmt * stmt
and literal =
    LitString of string
  | LitBool of bool
  | LitChar of char
  | LitInt of int
  | LitArray of exp list
  | LitPair
  | Null
(* for a particular field in function definition *)
and field = ty * symbol
and function_dec = FuncDec of ty * symbol * field list * stmt * pos
and t = function_dec list * stmt

(* Local Variables: *)
(* End: *)
