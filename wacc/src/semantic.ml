module A = Ast_v2;;
module S = Symbol;;
open Ast_v2;;
open Env;;
open Printf;;

exception SemanticError of string * A.pos
exception TypeMismatch of A.ty * A.ty * A.pos
exception UnknownIdentifier of A.symbol * A.pos
exception UnexpectedError of string * A.pos
let semantic_error_code = 200

let pos_lnum_cnum pos =
  let open Lexing in
  let lnum = pos.pos_lnum in
  let cnum = (pos.pos_cnum - pos.pos_bol + 1) in
  (lnum, cnum)

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

let var_name ident = match ident with
  | IdentExp name -> name
  | _ -> invalid_arg "not a variable"

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
  | _ -> invalid_arg ("Not a variable: " ^ name)

let check_type expected actual pos =
  if not (eq_type expected actual) then
    raise (TypeMismatch(expected, actual, pos))
  else ()

let is_var table name =
  match Symbol.lookup name table with
  | FuncEntry (retty, argtys) -> false
  | _ -> true

let is_heap_type  = function
  | A.ArrayTy _ | A.PairTy (_, _) -> true
  | _ -> false

let is_global_env (env) = match Symbol.parent env with
  | Some t -> (match Symbol.parent t with
      | None -> true | _ -> false)
  | _ -> false

let rec binop_type = function
  | (A.GeOp | A.GtOp | A.LeOp | A.LtOp |
     A.NeOp | A.EqOp | A.AndOp | A.OrOp ) -> A.BoolTy
  | _ -> A.IntTy

let rec check_function_call table fname exps pos = try
    match S.lookup fname table with
    | FuncEntry (retty, argtys) -> begin
        let exp_tys = List.map (check_exp table) exps in
        if argtys = exp_tys then retty else (
          raise (SemanticError ("Function call type mismatch", pos)))
      end
    | _ -> raise (SemanticError ("Not a valid function", pos))
  with
  | Not_found -> raise (UnknownIdentifier (fname, pos))

and check_exp (table: env) (exp: A.exp): ty = begin
  let (exp, pos) = exp in
  match exp with
  | IdentExp    (name) -> (
      try var_type table name
      with
      | Not_found -> raise (UnknownIdentifier (name, pos)))
  | LiteralExp  (literal) -> begin match literal with
      | LitBool _ -> BoolTy
      | LitChar _ -> CharTy
      | LitString _ -> StringTy
      | LitInt  _ -> IntTy
      | LitArray [] -> NullTy
      | LitArray (x::xs) -> ArrayTy (check_exp table x)
      | LitPair (f, s) -> PairTy ((check_exp table f), (check_exp table s))
      | LitNull      -> PairTyy
    end
  | BinOpExp    (exp, binop, exp') -> (
      let lty = check_exp table exp in
      let rty = check_exp table exp' in
      let () = (match binop with
       | (A.OrOp | A.AndOp ) ->
         (if (not (eq_type A.BoolTy lty) || not (eq_type A.BoolTy rty))
         then raise (SemanticError(("|| and && can only be applied on boolean types." ), pos)))
       | (A.MinusOp | A.ModOp | A.PlusOp | A.TimesOp | A.DivideOp ) ->
         (if (not (eq_type A.IntTy lty) || not (eq_type A.IntTy rty))
         then raise (SemanticError(("Numerical operations can only be applied on Integers." ), pos)))
       | (A.EqOp | A.NeOp ) -> ()
       | _ -> (
           if not(is_comparable (check_exp table exp)) then
             raise (SemanticError(("Inequality operations cannot be applied on strings, pairs and arrays." ), pos))
           )) in
      binop_type binop;)
  | UnOpExp     (unop, exp) -> (
      let ty = check_exp table exp in
      let expected_ty = (unop_arg_type unop) in
      check_type expected_ty ty pos;
      unop_ret_type unop)
  | NullExp -> PairTyy
  | NewPairExp  (exp, exp') -> begin
      let fst_ty = check_exp table exp in
      let snd_ty = check_exp table exp' in
      PairTy (fst_ty, snd_ty)
    end
  | CallExp     (fname, exps) -> begin
      try
        let (retty, argtys) = function_type table fname in
        let exp_tys = List.map (check_exp table) exps in
        if argtys = exp_tys then retty else (
          raise (SemanticError ("Function call type mismatch", pos)))
      with
      | Not_found -> raise (UnknownIdentifier (fname, pos))
    end
  | ArrayIndexExp (name, exps) -> (match var_type table name with
      | ArrayTy ty -> ty
      | StringTy -> CharTy
      | _ -> raise  (SemanticError ("Only indexing on array is supported", pos)))
  | FstExp (exp) -> begin
      let ty = check_exp table exp in
      match ty with
      | PairTy (t, t') -> t
      | _ -> type_mismatch PairTyy ty pos
    end
  | SndExp (exp) -> begin
      let ty = check_exp table exp in
      match ty with
      | PairTy (t, t') -> t'
      | _ -> type_mismatch PairTyy ty pos
    end
end

and check_stmt env stmt =
  let check_in_this_scope = check_exp env in
  let env' = S.new_scope env in
  let check_with_new_scope = check_stmt env' in
  let (stmt, pos) = stmt in
  match stmt with
  | SeqStmt ((RetStmt _, _), stmtlist) -> begin
      if (is_global_env env) then
        raise (SemanticError ("Return early in global", pos))
      else
        raise (A.SyntaxError ("Junk after return"))
    end
  | SeqStmt (stmt, stmtlist) ->
    (let env' = check_stmt env stmt in
     (check_stmt env' stmtlist))
  | AssignStmt ((IdentExp (name), _), exp) -> begin
      try
        if not (is_var env name) then raise (SemanticError ("Not a variable", pos))
        else
          let ty = var_type env name in
          let ty' = check_exp env exp in
          if eq_type ty ty' then env else raise (TypeMismatch (ty, ty', pos))
      with Not_found -> raise (SemanticError ("Variable not found: " ^ name, pos))
    end
  | AssignStmt   (lhs, rhs) -> begin
      try
        let ty = check_exp env lhs in
        let ty' = check_exp env rhs in
        check_type ty ty' pos; env
      with Not_found -> raise (SemanticError ("Variable not found", pos))
    end
  | IfStmt       (exp, exp', exp'') -> begin
      ignore(check_in_this_scope exp);
      let ty = check_exp env exp in
      check_type BoolTy ty pos;
      check_with_new_scope exp''
    end
  | WhileStmt    (exp, exp') -> begin
      let ty = check_exp env exp in
      check_type BoolTy ty pos;
      ignore(check_with_new_scope exp');
      env
    end
  | ExitStmt     (exp) -> begin
   check_type IntTy (check_exp env exp) pos;
   env;
   end
  | VarDeclStmt  (ty, symbol, exp) -> begin
      let ty' = check_exp env exp in
      let _ = (match S.lookup_opt' symbol env with
          | Some _ -> raise (SemanticError ("redeclared variable: " ^ symbol, pos))
          | None -> ()) in
      let env' = S.insert symbol (VarEntry (ty, None)) env in
      check_type ty ty' pos; env'
    end
  | SkipStmt -> env
  | PrintStmt    (_, exp) -> ignore(check_in_this_scope exp); env
  | RetStmt      (exp) -> begin
      let ty = check_in_this_scope exp in
      Symbol.insert "$result" (VarEntry (ty, None)) env
    end
  | ReadStmt     (exp) -> begin
      let ty = check_exp env exp in
       let check_read_ty = function
         | (A.IntTy | A.CharTy) -> ()
         | _ -> raise (SemanticError ("Unsupported read input type", pos)) in
       check_read_ty ty; env
    end
  | FreeStmt     (exp) -> begin
      let rty = check_exp env exp in
      if (not (is_heap_type rty)) then
        raise (SemanticError ("Can only free heap allocated data", pos))
      else env
    end
  | BlockStmt    (stmt) -> begin
      ignore(check_with_new_scope stmt);
      env
    end
and check_function_decls decls = begin
    let ctx = ref Symbol.empty in
    ignore(List.iter (fun x -> (match x with
        | FuncDec (ty, name, args, body), pos ->
          (try
             let _ = function_type !ctx name in
             raise (SemanticError ("Function redefined", pos))
           with
           | Not_found -> ());
          ctx := Symbol.insert name (FuncEntry (ty, List.map (fun (ft, name) -> ft) args)) !ctx
      )) decls);
    let check_dec decl = (match decl with
        | FuncDec (ty, name, args, body), pos -> begin
            let inner_ctx = ref (Symbol.new_scope !ctx) in
            let () = List.iter (fun x -> inner_ctx := (let (t, name) = x in Symbol.insert name (VarEntry (t, None)) !inner_ctx)) args in
            ignore(check_stmt !inner_ctx body)
          end) in
    ignore(List.map check_dec decls);
  end

let rec check_prog (decs, stmt) =
  try
    let table = baseenv in
    let table' = Symbol.new_scope (add_function_declarations table decs) in
    ignore(check_stmt table' stmt);
  with
  | TypeMismatch (expected, actual, pos) -> begin
      let (lnum, cnum) = pos_lnum_cnum pos in
      let open Prettyprint in
      fprintf stderr "Near %d:%d\n" lnum cnum;
      fprintf stderr "TypeMismatch; expected %s, but got %s\n";
        (* (prettyprint_type expected)
         * (prettyprint_type actual); *)
      exit(semantic_error_code);
    end
  | UnknownIdentifier (ident, pos) -> begin
      let (lnum, cnum) = pos_lnum_cnum pos in
      fprintf stderr "Near %d:%d\n" lnum cnum;
      fprintf stderr "Unknown Identifier %s\n" ident;
      exit(semantic_error_code);
    end
  | SemanticError (msg, pos) -> begin
      let (lnum, cnum) = pos_lnum_cnum pos in
      fprintf stderr "Near %d:%d\n" lnum cnum;
      fprintf stderr "%s\n" msg;
      exit(semantic_error_code);
    end

and add_function_declarations env ff =
  let env' = ref env in
  List.iter (fun f -> (let A.FuncDec (ty, ident, fields, stmt), pos = f in
                       let tys = List.map fst fields in
                       (match Symbol.lookup_opt ident !env' with
                        | Some _ -> raise (SemanticError ("Function " ^ ident ^  " has been redefined", pos))
                        | _ -> ());
                       env' := Symbol.insert ident (FuncEntry (ty, tys)) !env'
                      )) ff;
  match ff with
  | [] -> env
  | (f::fs) -> begin match f with
      | A.FuncDec (ty, ident, fields, stmt), pos ->
        let env'' = List.fold_left (fun env (ty, ident) -> Symbol.insert ident (VarEntry (ty, None)) env) !env' fields in
        let env''' = check_stmt env'' stmt in
        let rt = var_type env''' "$result" in
        if (not (eq_type rt ty)) then raise (SemanticError ("Result type mismatch", pos))
        else !env'
    end

let translate_prog (decs, stmt) =
  (* let frame = Translate.new_frame "main" in
   * let table = baseenv in
   * let table' = Symbol.new_scope (add_function_declarations table decs) in *)
  (* ignore(translate table' frame stmt) *)
  ()
