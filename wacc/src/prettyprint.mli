open Ast_v2;;

type fmt = Format.formatter;;

val prettyprint_type : fmt -> ty -> unit
val prettyprint_literal : fmt -> literal -> unit
val prettyprint_binop : fmt -> binop -> unit
val prettyprint_unop : fmt -> unop -> unit
val prettyprint_exp : fmt -> exp' -> unit
val prettyprint_stmt : fmt -> stmt' -> unit
