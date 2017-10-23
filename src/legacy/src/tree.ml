(* The Tree IR, similar to that from the TIGER compiler in
   'Modern Compiler Implementation in ML`
 *)
module Tree = struct
  type exp =
    | Const of int
    | Name of Temp.label
    | Temp of Temp.temp
    | Binop of exp * exp
    | Mem of exp
    | Call * Temp.label * exp list
  type stmt =
    | Move of exp * exp
    | Exp of exp
    | Jump * exp * Temp.label
    | CJump of relop * exp * exp * Temp.label * Temp.label
    | Seq of stm * stm
    | Label of Temp.label
  type binop =
    | Plus
    | Minus
    | Mul
    | Div
    | And
    | Or
    | Xor
    | Lshift
    | Rshift
    | Arshift
  type relop =
    | Eq
    | Ne
    | Lt
    | Gt
    | Le
    | Ge
    | Ult
    | Ule
    | Ugt
    | Uge
end
