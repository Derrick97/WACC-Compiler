module IL = Il;;
module F = Arm;;
module A = Ast_v2;;

type frame
type ctx = Env.env
type temp

val trans_prog: ctx -> A.t -> out_channel -> unit
