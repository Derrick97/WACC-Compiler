module A = Ast;;

type access
type frame
type frag
type exp
type size =
  | BYTE
  | WORD

val new_frame:  string -> frame

val trans_unop: A.unop -> exp -> exp
val trans_binop: A.binop -> exp -> exp -> exp

val trans_lit:  A.literal -> exp
val trans_array: access -> exp list -> exp
val trans_var: access -> exp

val trans_assign: exp -> exp -> exp

val trans_ifelse: exp -> exp -> exp -> exp
val trans_while: exp -> exp -> exp
val trans_seq: exp -> exp -> exp
val trans_call: string -> exp list -> exp
val access_of_exp: exp -> access

val trans_noop: exp
val allocate_local: frame -> size -> access
