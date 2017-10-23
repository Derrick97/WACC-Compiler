(* Translate module translates the AST to some IR *)
module Translate = sig
  val translate_stmt (ast: Ast.stmt) -> unit
  val translate_exp (ast: Ast.exp) -> unit
  val translate_decs (ast: Ast.dec list) -> unit
end
