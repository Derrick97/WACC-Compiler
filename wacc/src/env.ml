module A = Ast_v2;;

type access =
  | InFrame of int * int
  | InReg of Temp.temp

type enventry =
  | VarEntry of  A.ty * access option      (* variable *)
  | FuncEntry of A.ty * A.ty list (* types of params * type of result *)

type env = enventry Symbol.table
