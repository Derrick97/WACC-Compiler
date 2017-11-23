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
        | OrOp -> LiteralExp (LitBool(a||b))
        | _ -> assert false
        end
      | _ -> exp'
      in (simple_exp', pos)
    end
  | _ -> exp
