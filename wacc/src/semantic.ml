module A = Ast;;
module S = Symbol;;
module T = Translate;;

open Ast;;
open Env;;

exception SemanticError of string * A.pos
exception TypeMismatch of A.ty * A.ty * A.pos
exception UnknownIdentifier of A.symbol * A.pos
exception UnexpectedError of string * A.pos
exception SomeError of string

(* UnOp ArgType ReturnType *)
let unop_types = [
  (NotOp, BoolTy, BoolTy);
  (NegOp, IntTy, IntTy);
  (LenOp, ArrayTy NullTy, IntTy);
  (OrdOp, CharTy, IntTy);
  (ChrOp, IntTy, CharTy)
]

let unop_arg_type = function
  | NotOp -> BoolTy
  | NegOp -> IntTy
  | LenOp -> ArrayTy NullTy
  | OrdOp -> CharTy
  | ChrOp -> IntTy

let unop_ret_type = function
  | NotOp -> BoolTy
  | NegOp -> IntTy
  | LenOp -> IntTy
  | OrdOp -> IntTy
  | ChrOp -> CharTy

type env = enventry Symbol.table
let baseenv = Symbol.empty

let type_mismatch expected actual pos =
  raise (TypeMismatch (expected, actual, pos))

let var_name = function
  | IdentExp (name, pos) -> name
  | _ -> invalid_arg "Not a var!"

let is_comparable = function
  | (BoolTy | PairTy (_ , _) | ArrayTy(_)) -> false
  | _ -> true

let built_in_functions =
  [("chr", FuncEntry (CharTy, [IntTy]));
   ("ord", FuncEntry (IntTy, [CharTy]));
   ("print_int", FuncEntry (IntTy, [IntTy]));
   ("print_bool", FuncEntry (IntTy, [BoolTy]));
   ("print_char", FuncEntry (IntTy, [CharTy]));
  ]

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

let rec function_type table name =
  match S.lookup name table with
  | FuncEntry (rety, argtys) -> (rety, argtys)
  | VarEntry _ ->
    begin
      match S.parent table with
      | Some table -> function_type table name
      | None -> raise Not_found
    end

let var_type table name =
  match Symbol.lookup name table with
  | VarEntry (ty, _) -> ty
  | _ -> invalid_arg "not a variable"

let is_var table name =
  match Symbol.lookup name table with
  | FuncEntry (retty, argtys) -> false
  | _ -> true

let is_heap_type  = function
  | A.ArrayTy _ | A.PairTy (_, _) -> true
  | _ -> false

let is_global_env (env) = match Symbol.parent env with
  | Some t -> (match Symbol.parent t with
    | None -> true
    | _ -> false)
  | _ -> false

let rec binop_type = function
  | (A.GeOp | A.GtOp | A.LeOp | A.LtOp |
           A.NeOp | A.EqOp | A.AndOp | A.OrOp ) -> A.BoolTy
  | _ -> A.IntTy
and expect exp table binop pos =
   (match binop with
   | (A.OrOp | A.AndOp ) -> A.BoolTy
   | (A.MinusOp | A.ModOp | A.PlusOp | A.TimesOp | A.DivideOp ) -> A.IntTy
   | (A.EqOp | A.NeOp ) -> check_exp table exp
   | _ -> (
    if not(is_comparable (check_exp table exp)) then
    raise (SemanticError(("Unexpected type" ), pos))
    else check_exp table exp;))
and check_function_call table fname exps pos = try
    match S.lookup fname table with
    | FuncEntry (retty, argtys) -> begin
        let exp_tys = List.map (check_exp table) exps in
        if argtys = exp_tys then retty else (
          raise (SemanticError ("Function call type mismatch", pos)))
      end
    | _ -> raise (SemanticError ("Not a valid function", pos))
  with
  | Not_found -> raise (UnknownIdentifier (fname, pos))
(* Return type of an expression, will raise TypeMismatch if type mismatch *)
and check_exp (table: env) (exp: A.exp): ty = begin
  match exp with
  | IdentExp    (name, pos) -> (
      try var_type table name
      with
        | Not_found -> raise (UnknownIdentifier (name, pos)))
  | LiteralExp  (literal, pos) -> begin match literal with
      | LitBool _ -> BoolTy
      | LitChar _ -> CharTy
      | LitString _ -> StringTy
      | LitInt  _ -> IntTy
      | LitArray [] -> NullTy
      | LitArray (x::xs) -> ArrayTy (check_exp table x)
      | LitPair (f, s) -> PairTy ((check_exp table f), (check_exp table s))
      | Null      -> PairTyy
    end
  | BinOpExp    (exp, binop, exp', pos) -> (
      let lty = check_exp table exp in
      let rty = check_exp table exp' in
      if eq_type (expect exp table binop pos) lty && eq_type (expect exp table binop pos) rty
      then binop_type binop
      else raise (SemanticError("Unexpected type comparison", pos)))
  | UnOpExp     (unop, exp, pos) -> (
      let ty = check_exp table exp in
      let expected_ty = (unop_arg_type unop) in
      if eq_type ty expected_ty then unop_ret_type unop else raise (TypeMismatch (ty, expected_ty, pos)))
  | NullExp     pos -> PairTyy
  | NewPairExp  (exp, exp', pos) -> begin
      let fst_ty = check_exp table exp in
      let snd_ty = check_exp table exp' in
      PairTy (fst_ty, snd_ty)
    end
  | CallExp     (fname, exps, pos) -> begin
    try
    let (retty, argtys) = function_type table fname in
    let exp_tys = List.map (check_exp table) exps in
    if argtys = exp_tys then retty else (
        raise (SemanticError ("Function call type mismatch", pos)))
    with
    | Not_found -> raise (UnknownIdentifier (fname, pos))
    end
  | ArrayIndexExp (name, exps, pos) -> let ty = match var_type table name with
      | ArrayTy ty -> ty
      | StringTy -> CharTy
      | ty -> raise  (SemanticError ("Only indexing on array is supported", pos))
    in ty
  | FstExp (exp, pos) -> begin let ty = check_exp table exp in
    match ty with
    | PairTy (t, t') -> t
    | _ -> raise (TypeMismatch (PairTyy, ty, pos))
    end
  | SndExp (exp, pos) -> begin let ty = check_exp table exp in
    match ty with
    | PairTy (t, t') -> t'
    | _ -> raise (TypeMismatch (PairTyy, ty, pos))
    end
end

and translate_exp (env: env) (exp: A.exp): T.exp = begin
  let tr = translate_exp env in
  match exp with
  | IdentExp    (name, pos) -> T.trans_noop
  | LiteralExp  (literal, pos) ->
     T.trans_lit literal
  | BinOpExp    (exp, binop, exp', pos) -> T.trans_binop binop (tr exp) (tr exp')
  | UnOpExp     (unop, exp, pos) -> T.trans_unop unop (translate_exp env exp)
  | _ -> assert false
end

and translate (env: env)
    (frame: T.frame)
    (stmt: stmt): (T.exp * env) =
  let tr x = (translate_exp env x) in
  match stmt with
  | SeqStmt (stmt, stmtlist) -> begin
      let exp, env' = translate env frame stmt in
      let exp', env'' = (translate env frame stmtlist) in
      (T.trans_seq exp exp', env'')
    end
  | AssignStmt   (lhs, rhs, _) -> begin
      (T.trans_assign ((tr lhs)) (tr rhs), env)
    end
  | IfStmt       (cond, then_exp, else_exp, _) -> begin
      let env' = Symbol.new_scope env in
      let then_exp, _ = translate env' frame then_exp in
      let else_exp, _ = translate env' frame else_exp in
      T.trans_ifelse (tr cond) (then_exp) (else_exp), env
    end
  | WhileStmt    (cond, body_stmt, _) -> begin
      let env' = Symbol.new_scope env in
      T.trans_while (tr cond) (fst (translate env' frame body_stmt)),
      env
    end
  | ExitStmt     (exp, _) -> begin
      (T.trans_call "wacc_exit" [tr exp]), env
    end
  | VarDeclStmt  (ty, name, exp, _) -> begin
      let size = match ty with
        | CharTy | BoolTy -> T.BYTE
        | IntTy -> T.WORD
        | _ -> assert false in
      let local_var = T.allocate_local frame size in
      let env' = Symbol.insert name (VarEntry (ty, Some local_var)) env in
      T.trans_assign (T.trans_var local_var) (tr exp), env'
    end
  | SkipStmt      _ -> T.trans_noop, env
  | PrintStmt    (newline, exp, _) -> begin
      if newline then
        T.trans_call "wacc_println" [tr exp], env
      else
        T.trans_call "wacc_print" [tr exp], env
    end
  | RetStmt      (exp, _) -> failwith "TODO"
  | ReadStmt     (exp, _) -> T.trans_call "wacc_read" [tr exp], env
  | FreeStmt     (exp, _) -> T.trans_call "wacc_free" [tr exp], env
  | BlockStmt    (stmt, _) -> begin
      let env' = Symbol.new_scope env in
      translate env' frame stmt
    end

and check_stmt env stmt =
  let check_in_this_scope = check_exp env in
  let env' = S.new_scope env in
  let check_with_new_scope = check_stmt env' in
  match stmt with
  | SeqStmt (RetStmt (_, pos), stmtlist) -> begin
      if (is_global_env env) then
        raise (SemanticError ("Return early in global", pos))
      else
        raise (A.SyntaxError ("Junk after return"))
    end
  | SeqStmt (stmt, stmtlist) ->
    (let env' = check_stmt env stmt in
     (check_stmt env' stmtlist))
  | AssignStmt (IdentExp (name, _), exp, pos) -> begin
      try
        if not (is_var env name) then raise (SemanticError ("Not a variable", pos))
        else
          let ty = var_type env name in
          let ty' = check_exp env exp in
          if eq_type ty ty' then env else raise (TypeMismatch (ty, ty', pos))
      with Not_found -> raise (SemanticError ("Variable not found: " ^ name, pos))
    end
  | AssignStmt   (lhs, rhs, pos) -> begin
      try
        let ty = check_exp env lhs in
        let ty' = check_exp env rhs in
        if eq_type ty ty' then env else raise (TypeMismatch (ty, ty', pos))
      with Not_found -> raise (SemanticError ("Variable not found", pos))
    end
  | IfStmt       (exp, exp', exp'', pos) -> begin
      ignore(check_in_this_scope exp);
      let ty = check_exp env exp in
      if eq_type ty BoolTy then (check_with_new_scope exp'')
      else raise (TypeMismatch (BoolTy, ty, pos))
    end
  | WhileStmt    (exp, exp', pos) -> begin
      let ty = check_exp env exp in
      if eq_type ty BoolTy then check_with_new_scope exp' else raise (TypeMismatch (BoolTy, ty, pos))
    end
  | ExitStmt     (exp, pos) -> (if (check_exp env exp) != IntTy then raise (SemanticError ("Exit code is not int", pos)) else env)
  | VarDeclStmt  (ty, symbol, exp, pos) ->
    begin
      let _ = check_exp env exp in
      let _ = (match S.lookup_opt' symbol env with
        | Some _ -> raise (SemanticError ("redeclared variable: " ^ symbol, pos))
        | None -> ()) in
      let env' = S.insert symbol (VarEntry (ty, None)) env in
      let ty' = check_exp env' exp in
      if eq_type ty ty' then env' else raise (TypeMismatch (ty, ty', pos))
    end
  | SkipStmt      pos -> env
  (* TODO someof these need to be type check *)
  | PrintStmt    (_, exp, pos) -> ignore(check_in_this_scope exp); env
  | RetStmt      (exp, pos) -> (let ty = check_in_this_scope exp in
                                Symbol.insert "$result" (VarEntry (ty, None)) env
                               )
  | ReadStmt     (exp, pos) -> (let ty = check_exp env exp in
                                let check_read_ty = function
                                  | (A.IntTy | A.CharTy) -> ()
                                  | _ -> raise (SemanticError ("Unsupported read input type", pos)) in
                                check_read_ty (ty); env
                               )
  | FreeStmt     (exp, pos) -> (let rty = check_exp env exp in
                                if (not (is_heap_type rty)) then
                                  raise (SemanticError ("Can only free heap allocated data", pos))
                                else env)
  | BlockStmt    (stmt, pos) -> (ignore(check_with_new_scope stmt);
                                 env)
and check_function_decls decls =
  begin
  let ctx = ref Symbol.empty in
  ignore(List.iter (fun x -> (match x with
      | FuncDec (ty, name, args, body, pos) ->
        (try
            let _ = function_type !ctx name in
            raise (SemanticError ("Function redefined", pos))
         with
          | Not_found -> ());
        ctx := Symbol.insert name (FuncEntry (ty, List.map (fun (ft, name) -> ft) args)) !ctx
    )) decls);
  let check_dec decl = (match decl with
      | FuncDec (ty, name, args, body, pos) ->
        begin
          let inner_ctx = ref (Symbol.new_scope !ctx) in
          let () = List.iter (fun x -> inner_ctx := (let (t, name) = x in Symbol.insert name (VarEntry (t, None)) !inner_ctx)) args in
          ignore(check_stmt !inner_ctx body)
        end) in
  ignore(List.map check_dec decls);
  ()
  end

let rec add_function_declarations env ff =
  let env' = ref env in
  List.iter (fun f -> (let A.FuncDec (ty, ident, fields, stmt, pos) = f in
                       let tys = List.map fst fields in
                       (match Symbol.lookup_opt ident !env' with
                       | Some _ -> raise (SemanticError ("Function " ^ ident ^  " has been redefined", pos))
                       | _ -> ());
                       env' := Symbol.insert ident (FuncEntry (ty, tys)) !env'
                      )) ff;
  match ff with
  | [] -> env
  | (f::fs) -> begin match f with
      | A.FuncDec (ty, ident, fields, stmt, pos) ->
        let env'' = List.fold_left (fun env (ty, ident) -> Symbol.insert ident (VarEntry (ty, None)) env) !env' fields in
        let env''' = check_stmt env'' stmt in
        let rt = var_type env''' "$result" in
        if (not (eq_type rt ty)) then raise (SemanticError ("Result type mismatch", pos))
        else !env'
    end
