module A = Ast;;
type access = unit

type enventry =
  | VarEntry of  A.ty * access      (* variable *)
  | FuncEntry of A.ty * A.ty list (* types of params * type of result *)

type env = enventry Symbol.table
