module A = Ast
module S = Symbol

open Ast

exception SemanticError of string * A.pos
exception TypeMismatch of A.ty * A.ty * A.pos
exception UnknownIdentifier of A.symbol * A.pos
exception UnexpectedError of string * A.pos

type enventry =
  | VarEntry of A.ty               (* variable *)
  | FuncEntry of A.ty * A.ty list  (* types of params * type of result *)

type 'a table = 'a S.table

let type_mismatch expected actual pos =
  raise (TypeMismatch (expected, actual, pos))

let var_name = function
  | IdentExp (name, pos) -> name
  | _ -> raise (Invalid_argument "not a var!")

let rec eq_type t1 t2 = match (t1, t2) with
  | (BoolTy, BoolTy) -> true
  | (IntTy, IntTy) -> true
  | (StringTy, StringTy) -> true
  | (CharTy, CharTy) -> true
  | (PairTy (t1, t2), PairTyy) -> true
  | (PairTyy, PairTy (t1, t2)) -> true
  | (PairTyy, PairTyy) -> true
  | (PairTy (t1', t2'), PairTy (t1'', t2'')) -> (eq_type t1' t1'') && (eq_type t2' t2'')
  | (ArrayTy t, ArrayTy t') -> eq_type t t'
  | (_, NullTy) -> true
  | (NullTy, _) -> true
  | (_, _) -> false

let rec lookup_function table name =
  match S.lookup name table with
  | FuncEntry (rety, argtys) -> FuncEntry (rety, argtys)
  | VarEntry _ ->
    begin
      match S.parent table with
      | Some table -> lookup_function table name
      | None -> raise Not_found
    end

let var_type table name =
  match Symbol.lookup name table with
  | VarEntry ty -> ty
  | _ -> raise (Invalid_argument "not a variable")

let is_var table name =
  match Symbol.lookup name table with
  | FuncEntry (retty, argtys) -> false
  | _ -> true

let rec exp_type table exp =
  let exp_type' = exp_type table in
  match exp with
  | IdentExp    (name, pos) -> var_type table name
  | LiteralExp  (literal, pos) ->
    begin
      match literal with
      | LitBool _ -> BoolTy
      | LitChar _ -> CharTy
      | LitString _ -> StringTy
      | LitInt  _ -> IntTy
      | LitArray [] -> NullTy
      | LitArray (x::xs) -> ArrayTy (exp_type table x)
      | LitPair (f, s) -> PairTy ((exp_type table f), (exp_type table s))
      | Null      -> PairTyy
    end
  | BinOpExp    (exp, binop, exp', pos) -> binop_type binop
  | UnOpExp     (unop, exp, pos) -> A.unop_ret_type unop
  | NullExp     (pos) -> PairTyy
  | NewPairExp  (exp, exp', pos) -> PairTy ((exp_type table exp), (exp_type table exp'))
  | CallExp     (fname, exps, pos) ->
    begin
    try
      let FuncEntry (retty, argt) = lookup_function table fname in retty (* TODO complete the pattern matching *)
    with
    | Not_found -> raise (UnknownIdentifier (fname, pos))
    end
  | SndExp  (exp, pos) ->
    begin
      let ty = exp_type' exp in
      match ty with
      | PairTy (t, t') -> t'
      | _ -> raise (TypeMismatch (PairTyy, ty, pos))
    end
  | FstExp  (exp, pos) ->
    begin
      let ty = exp_type' exp in
      match ty with
      | PairTy (t, t') -> t
      | _ -> raise (TypeMismatch (PairTyy, ty, pos))
    end
  | _ -> raise (SemanticError ("ERROR checking expression on RHS", Lexing.dummy_pos))
and binop_type = function
  | (A.GeOp | A.GtOp | A.LeOp | A.LtOp |
           A.NeOp | A.EqOp | A.AndOp | A.OrOp ) -> A.BoolTy
  | _ -> A.IntTy
and check_function_call table fname exps pos =
  try
    match S.lookup fname table with
    | FuncEntry (retty, argtys) ->
      begin
        let exp_tys = List.map (exp_type table) exps in
        if argtys == exp_tys then table else raise (SemanticError ("func call type mismatch", pos))
      end
    | _ -> raise (SemanticError ("not a valid function", pos))
  with
  | Not_found -> raise (UnknownIdentifier (fname, pos))
(* Return type of an expression, will raise TypeMismatch if type mismatch *)
let rec check_exp (table: 'a S.table) exp =
  let check_in_this_scope = check_exp table in
  let table' = S.new_scope table in
  let check_with_new_scope = check_exp table' in
  match exp with
  | IdentExp    (name, pos) ->
    begin
        try
        let _ = S.lookup name table in table
        with
        | Not_found -> raise (UnknownIdentifier (name, pos))
    end
  | LiteralExp  (literal, pos) -> table
  | BinOpExp    (exp, binop, exp', pos) ->
    begin
      let lty = exp_type table exp in
      let rty = exp_type table exp' in
      if eq_type lty rty then table else raise (TypeMismatch (lty, rty, pos))
    end
  | UnOpExp     (unop, exp, pos) ->
    begin
      check_in_this_scope exp;
      let ty = exp_type table exp in
      let expected_ty = (A.unop_arg_type unop) in
      if eq_type ty expected_ty then table else raise (TypeMismatch (ty, expected_ty, pos))
    end
  | NullExp     pos -> table
  | NewPairExp  (exp, exp', pos) -> table
  | CallExp     (symbol, exps, pos) -> check_function_call table symbol exps pos
  | ArrayIndexExp _ -> table
  | FstExp _ -> table
  | SndExp _ -> table
and check_stmt table stmt =
  let check_in_this_scope = check_exp table in
  let table' = S.new_scope table in
  let check_with_new_scope = check_stmt table' in
  match stmt with
  | SeqStmt ([])   -> table
  | SeqStmt (x::xs) -> (let table' = check_stmt table x in
                       (check_stmt table' (SeqStmt xs)))
  | AssignStmt   (IdentExp (name, _), exp, pos) ->
    begin
      try
        if not (is_var table name) then raise (SemanticError ("Not a variable", pos))
        else
          let ty = var_type table name in
          let ty' = exp_type table exp in
          if eq_type ty ty' then table else raise (TypeMismatch (ty, ty', pos))
      with Not_found -> raise (SemanticError ("variable not found", pos))
    end
  | AssignStmt   (lhs, rhs, pos) ->
    begin
      try
        let ty = exp_type table lhs in
        let ty' = exp_type table rhs in
        if eq_type ty ty' then table else raise (TypeMismatch (ty, ty', pos))
      with Not_found -> raise (SemanticError ("variable not found", pos))
    end
  | IfStmt       (exp, exp', exp'', pos) ->
    begin
      ignore(check_in_this_scope exp);
      let ty = exp_type table exp in
      if eq_type ty BoolTy then (ignore(check_with_new_scope exp');
                                 check_with_new_scope exp'')
      else raise (TypeMismatch (BoolTy, ty, pos))
    end
  | WhileStmt    (exp, exp', pos) ->
    begin
      ignore(check_in_this_scope exp);
      let ty = exp_type table exp in
      if eq_type ty BoolTy then check_with_new_scope exp' else raise (TypeMismatch (BoolTy, ty, pos))
    end
  | ExitStmt     (exp, pos) -> check_in_this_scope exp
  | VarDeclStmt  (ty , symbol, exp, pos) ->
    begin
      let table' = S.insert symbol (VarEntry ty) table in
      let ty' = exp_type table' exp in
      if eq_type ty ty' then table' else raise (TypeMismatch (ty, ty', pos))
    end
  | SkipStmt      pos -> table
  (* TODO someof these need to be type check *)
  | PrintStmt    (exp, pos) -> check_in_this_scope exp
  | PrintLnStmt  (exp, pos) -> check_in_this_scope exp
  | RetStmt      (exp, pos) -> check_in_this_scope exp
  | ReadStmt     (exp, pos) -> check_in_this_scope exp
  | FreeStmt     (exp, pos) -> check_in_this_scope exp
  | BlockStmt    (exp, pos) -> (ignore(check_with_new_scope exp);
                               table)

