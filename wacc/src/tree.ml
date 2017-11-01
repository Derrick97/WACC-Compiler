(* The Tree IR, similar to that from the TIGER compiler in
   'Modern Compiler Implementation in ML`
 *)
type binop = PLUS | MINUS | MUL | DIV | AND | OR
           | LSHIFT | RSHIFT | ARSHIFT | XOR
and relop = EQ | NE | LT | GT | LE | GE | ULT | UGT | UGE
and exp =
  | Const of int * Ast.ty
  | Name of Temp.label
  | Temp of Temp.temp
  | Binop of binop * exp * exp
  | Mem of exp
  | Eseq of stmt * exp
  | Call of string * exp list
and stmt =
  | Move of exp * exp
  | Exp of exp
  | Jump of exp * Temp.label list
  | CJump of relop * exp * exp * Temp.label
  | Seq of stmt * stmt
  | Label of Temp.label
  | NoOp
