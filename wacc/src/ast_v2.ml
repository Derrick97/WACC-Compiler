exception SyntaxError of string

type pos = Lexing.position
and 'a identified = 'a * pos
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
and ident = symbol
and exp' =
  | IdentExp      of ident
  | ArrayIndexExp of ident * exp list
  | LiteralExp    of literal
  | BinOpExp      of exp * binop * exp
  | UnOpExp       of unop * exp
  | NullExp
  | NewPairExp    of exp * exp
  | CallExp       of ident * (exp list)
  | FstExp        of exp
  | SndExp        of exp
and exp = exp' identified
and stmt' =
  | SkipStmt
  | VarDeclStmt of ty * ident * exp
  | AssignStmt of exp * exp
  | ReadStmt of exp
  | FreeStmt of exp
  | ExitStmt of exp
  | PrintStmt of bool * exp
  | IfStmt of exp * stmt * stmt
  | WhileStmt of exp * stmt
  | BlockStmt of stmt
  | RetStmt of exp
  | SeqStmt of stmt * stmt
and stmt = stmt' identified
and literal =
    LitString of string
  | LitBool of bool
  | LitChar of char
  | LitInt of int
  | LitArray of exp list
  | LitPair of (exp * exp)
  | LitNull
  (* for a particular field in function definition *)
and field = ty * ident
and function_dec' = FuncDec of ty * ident * field list * stmt

and function_dec = function_dec' identified
and t = function_dec list * stmt
