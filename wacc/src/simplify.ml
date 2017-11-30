module A = Ast_v2

let rec simplify (exp: A.exp): A.exp =
  let open Ast_v2 in
  let (exp', pos) = exp in
  match exp' with
  | A.BinOpExp (lhs, op, rhs) -> begin
      let (lhs', pos) = simplify lhs in
      let (rhs', pos) = simplify rhs in
      let simple_exp' = match (lhs', rhs') with
      | (LiteralExp (LitInt(a)), LiteralExp (LitInt(b))) -> begin
        match op with
        | PlusOp -> LiteralExp (LitInt(a+b))
        | MinusOp -> LiteralExp (LitInt(a-b))
        | TimesOp -> LiteralExp (LitInt(a*b))
        | DivideOp -> LiteralExp (LitInt(a/b))
        | ModOp -> LiteralExp (LitInt(a mod b))
        | GeOp -> LiteralExp (LitBool(a>=b))
        | GtOp -> LiteralExp (LitBool(a>b))
        | EqOp -> LiteralExp (LitBool(a=b))
        | NeOp -> LiteralExp (LitBool(a<>b))
        | LtOp -> LiteralExp (LitBool(a<b))
        | LeOp -> LiteralExp (LitBool(a<=b))
        | _ -> assert false
        end
      | (LiteralExp (LitBool(a)), LiteralExp (LitBool(b))) -> begin
        match op with
        | AndOp -> LiteralExp (LitBool(a&&b))
        | OrOp  -> LiteralExp (LitBool(a||b))
        | _ -> assert false
        end
      | _ -> begin
        match op with
        | ModOp -> CallExp ("wacc_mod", [lhs; rhs])
        | DivideOp -> CallExp ("wacc_div", [lhs; rhs])
        | _ -> exp'
      end
      in (simple_exp', pos)
    end
  | A.UnOpExp (op, rhs) -> begin
    let (rhs', pos) = simplify rhs in
    let exp' = match rhs' with
    | LiteralExp (LitInt(a)) -> begin
      match op with
      | NegOp -> LiteralExp (LitInt(-a))
      | ChrOp -> LiteralExp (LitChar(Char.chr a))
      | _ -> assert false
      end
    | LiteralExp (LitBool(b)) -> begin
      match op with
      | NotOp ->  LiteralExp (LitBool(not b))
      | _ -> assert false
      end
    | _ ->
      match op with
      | LenOp -> CallExp ("wacc_len", [rhs])
      | ChrOp -> CallExp ("wacc_chr", [rhs])
      | OrdOp -> CallExp ("wacc_ord", [rhs])
      | _ -> exp'
    in (exp', pos)
    end
  | _ -> exp


let rec simplify_stmt (ast: A.stmt): A.stmt =
  let open Ast_v2 in
  let (stmt', pos) = ast in
  match stmt' with
  | A.IfStmt (cond, then_stmt, else_stmt) -> begin
    let (cond', pos) = cond in
    match cond' with
    | LiteralExp(LitBool(b)) -> if b then simplify_stmt then_stmt else simplify_stmt else_stmt
    | _ -> (IfStmt (cond, simplify_stmt then_stmt, simplify_stmt else_stmt), pos)
    end
  | A.WhileStmt (cond, body_stmt) -> begin
    let (cond', pos) = cond in
    match cond' with
    | LiteralExp(LitBool(b)) -> if b then ast else (SkipStmt, pos)
    | _ -> (WhileStmt (cond, simplify_stmt body_stmt), pos)
    end
  | A.ExitStmt (exit_code) -> (CallStmt (CallExp("wacc_exit", [exit_code]),pos),pos)
  | A.SkipStmt -> ast
  | A.CallStmt (exp) -> (CallStmt (simplify exp), pos)
  | A.VarDeclStmt (ty, ident, exp) -> (VarDeclStmt (ty, ident, simplify exp), pos)
  | A.AssignStmt (exp1, exp2) -> (AssignStmt (simplify exp1, simplify exp2), pos)
(*  | A.ReadStmt of exp
  | A.FreeStmt of exp
  | A.BlockStmt of stmt
  | A.RetStmt of exp*)
  | A.PrintStmt (newline, exp) -> (PrintStmt (newline, simplify exp), pos)
  | A.SeqStmt (stmt, stmtlist) -> (SeqStmt (simplify_stmt stmt, simplify_stmt stmtlist),pos)
  | _ -> ast
