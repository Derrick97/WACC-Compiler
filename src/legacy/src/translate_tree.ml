struct Translate_Tree = struct
  type state
  open Ast
  let translate_stmt state (stmt: Ast.stmt) -> Tree.stmt =
    | SkipStmt (_) -> ()
    | VarDeclStmt (ty, symbol, exp, pos) -> ()
    | AssignStmt (exp, exp, pos) -> ()
    | ReadStmt (exp, pos) -> ()
    | FreeStmt (exp, pos) -> ()
    | ExitStmt (exp, pos) -> ()
    | PrintStmt (exp, pos) -> ()
    | PrintLnStmt (exp, pos) -> ()
    | IfStmt (exp, stmt, stmt, pos) -> ()
    | WhileStmt (then_, stmt, pos) -> ()
    | BlockStmt (stmt, pos) -> ()
    | RetStmt (exp, pos) -> ()
    | SeqStmt stmts -> ()
  and translate_binop state exp -> Tree.exp
  and translate_unop state exp -> Tree.exp
  and translate_literal state lit -> Tree.exp =
    match lit with
    | LitString s -> ()
    | LitBool b -> Const (if b then 1 else 0)
    | LitChar c -> Const (ord c)
    | LitInt  i -> Const i
    | LitArray (indexing_exps) -> ()
    | LitPair (fst_exp, snd_exp) -> ()
    | Null -> ()
  and rec trans_exp state (ast: Ast.exp) -> Tree.exp =
    match ast with
    | IdentExp (ident, _) -> ()
    | ArrayIndexExp(ident, exps, _) -> ()
    | LiteralExp (lit, _) -> ()
    | BinOpExp (exp, binop, exp, _) -> ()
    | UnOpExp  (op, exp, _) -> ()
    | NullExp  (_) -> ()
    | NewPairExp  (leftexp, rightexp, _) -> ()
    | CallExp (name, args, _) -> ()
    | FstExp  (exp, _) -> ()
    | SndExp  (exp, _) -> ()
  and translate_decs state (decs: Ast.dec list) -> Tree.stmt
end
