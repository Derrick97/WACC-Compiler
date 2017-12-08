module A = Ast_v2

let rec simplify (exp: A.exp): A.exp =
  let open Ast_v2 in
  let (exp', pos) = exp in
  match exp' with
  | A.BinOpExp (lhs, op, rhs) -> begin
      let (lhs', pos) = simplify lhs in
      let (rhs', pos) = simplify rhs in
      let simple_exp' = (match (lhs', rhs') with
      | (LiteralExp (LitInt(a)), LiteralExp (LitInt(b))) -> begin
        match op with
        | PlusOp -> LiteralExp (LitInt(a+b))
        | MinusOp -> LiteralExp (LitInt(a-b))
        | TimesOp -> LiteralExp (LitInt(a*b))
        | DivideOp -> if b != 0 then LiteralExp (LitInt(a/b)) else CallExp("wacc_div", [simplify lhs; simplify rhs])
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
          | ModOp -> CallExp ("wacc_mod", [simplify lhs; simplify rhs])
          | DivideOp -> CallExp ("wacc_div", [simplify lhs; simplify rhs])
          | _ -> A.BinOpExp (simplify lhs, op, simplify rhs)
      end)
      in (simple_exp', pos)
    end
  | A.UnOpExp (op, rhs) -> begin
    let (rhs', pos) = simplify rhs in
    let exp' =  match op with
      | LenOp -> CallExp ("wacc_len", [rhs', pos])
      | ChrOp -> CallExp ("wacc_chr", [rhs', pos])
      | OrdOp -> CallExp ("wacc_ord", [rhs', pos])
      | _ -> exp'
    in (exp', pos)
  end
  | A.CallExp (fname, args) -> A.CallExp (fname, (List.map simplify args)), pos
  | A.FstExp e -> A.FstExp (simplify e), pos
  | A.SndExp e -> A.SndExp (simplify e), pos
  | A.NewPairExp (e,e') -> A.NewPairExp (simplify e, simplify e'), pos
  | A.ArrayIndexExp (name, args) -> A.ArrayIndexExp (name, List.map simplify args), pos
  | A.LiteralExp _  | A.IdentExp _ | NullExp -> exp


let rec simplify_stmt (ast: A.stmt): A.stmt =
  let open Ast_v2 in
  let (stmt', pos) = ast in
  (match stmt' with
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
  | A.ExitStmt (exit_code) -> (CallStmt (CallExp("wacc_exit", [simplify exit_code]),pos),pos)
  | A.VarDeclStmt (ty, ident, exp) ->  A.VarDeclStmt (ty, ident, simplify exp), pos
  | A.AssignStmt (exp, exp') -> A.AssignStmt (simplify exp, simplify exp'), pos
  | A.ReadStmt _ | SkipStmt | A.FreeStmt _ -> ast
  | A.PrintStmt (newline, exp) -> A.PrintStmt (newline, simplify exp), pos
  | A.BlockStmt stmt -> (A.BlockStmt (simplify_stmt stmt), pos)
  | A.RetStmt exp -> (A.RetStmt (simplify exp), pos)
  | A.SeqStmt (stmt, stmtlist) -> (A.SeqStmt (simplify_stmt stmt, simplify_stmt stmtlist), pos)
  | _ -> ast)
