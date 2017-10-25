(* Translate module translates the AST to some IR *)
val translate_stmt: Ast.stmt -> unit
val translate_exp:  Ast.exp -> unit
val translate_decs: Ast.dec list -> unit
