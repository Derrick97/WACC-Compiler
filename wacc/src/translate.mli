module A = Ast;;

type access
type frame
type frag
type exp

val new_frame:  string -> frame
val trans_binop: A.binop -> exp -> A.exp -> exp
val trans_unop: A.unop -> exp -> exp
val trans_lit:  A.literal -> exp
val trans_ifelse: exp -> exp -> exp -> exp
val trans_var: access -> exp
val trans_array: access -> exp list -> exp
val trans_assign: access -> exp -> exp
val trans_pair: access -> bool -> exp -> exp
val trans_while: exp -> exp
val trans_seq: exp -> exp -> exp
val allocate_local: frame -> A.ty -> access
