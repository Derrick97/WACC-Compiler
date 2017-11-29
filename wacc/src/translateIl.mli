module IL = Il;;
module F = Arm;;
module A = Ast_v2;;

type frame
type ctx = Env.env
type temp

val trans_stmt: ctx -> frame -> A.stmt -> IL.il list * ctx
val trans_exp:  ctx -> A.exp -> temp * IL.il list
val trans_call: ctx -> string -> temp list -> IL.il list
val trans_prog: ctx -> A.t -> out_channel -> IL.il list
