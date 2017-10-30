module A = Ast;;

type access
type frame
type frag

val new_frame:  string -> frame
val allocate_local: unit -> access
val trans_stmt: Semantic.env -> frame -> A.stmt -> frag
val trans_exp:  Semantic.env -> frame -> A.exp -> access
val trans_dec:  Semantic.env -> frame -> frame -> A.function_dec -> frag
val trans_lit:  frame -> A.literal -> access
