module A = Ast;;

type access
type frame
type frag

open Env;;

val new_frame:  string -> frame
val trans_stmt: enventry Symbol.table -> frame -> A.stmt -> frag
val trans_exp:  enventry Symbol.table -> frame -> A.exp -> access
val trans_dec:  enventry Symbol.table
  -> frame -> frame -> A.function_dec -> frag
val trans_lit:  frame -> A.literal -> access
