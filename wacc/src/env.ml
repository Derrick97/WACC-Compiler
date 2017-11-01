module A = Ast;;
type access

type enventry =
  | VarEntry of  A.ty * Translate.access option      (* variable *)
  | FuncEntry of A.ty * A.ty list (* types of params * type of result *)

type env = enventry Symbol.table
