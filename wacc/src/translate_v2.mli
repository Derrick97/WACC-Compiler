module A = Ast;;

type access
type frame
type frag
type exp
type stmt

type size = int

val new_frame:  string -> frame

val trans_unop: A.unop -> exp -> (stmt list * exp)
val trans_binop: A.binop -> exp -> exp -> (stmt list * exp)

val trans_lit:  A.literal -> exp
val trans_array: access -> exp list -> exp
val trans_var: access -> exp

val trans_assign: exp -> exp -> (stmt list * exp)

val trans_ifelse: exp -> (stmt list) -> (stmt list) -> (stmt list)
val trans_while: exp -> (stmt list) -> stmt list
val trans_seq: exp -> exp -> exp
val trans_call: string -> exp list -> exp
val access_of_exp: exp -> access

val trans_noop: exp
val allocate_local: frame -> size -> access
