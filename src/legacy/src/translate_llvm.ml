struct Translate_Llvm = struct
  let translate_stmt (ast: Ast.stmt) -> ()
  let translate_exp (ast: Ast.exp) -> ()
  let translate_decs (ast: Ast.dec list) -> ()
end
