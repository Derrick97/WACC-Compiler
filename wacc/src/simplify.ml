module A = Ast_v2

let rec simplify (exp: A.exp): A.exp =
  let (exp', pos) = exp in
  match exp' with
  | A.BinOpExp (lhs, op, rhs) -> begin
      let lhs' = simplify lhs in
      let rhs' = simplify rhs in
      exp
    end
  | _ -> failwith "TODO"


let simplify (ast: A.stmt): A.stmt = ast
