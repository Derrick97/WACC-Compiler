module A = Ast;;

type access =
  | InFrame of int * int

type enventry =
  | VarEntry of  A.ty * access option      (* variable *)
  | FuncEntry of A.ty * A.ty list (* types of params * type of result *)

type env = enventry Symbol.table
