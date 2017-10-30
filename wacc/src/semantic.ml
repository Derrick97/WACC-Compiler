module A = Ast
module S = Symbol

open Ast

exception SemanticError of string * A.pos
exception TypeMismatch of A.ty * A.ty * A.pos
exception UnknownIdentifier of A.symbol * A.pos
exception UnexpectedError of string * A.pos
exception SomeError of string

(* let var_name = function *)
(*   | IdentExp (name, _) -> name *)
(*   | ArrayIndexExp (name, _, _) -> name *)
(*   | _ -> raise (Invalid_argument "not a var") *)

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

type enventry =
  | VarEntry of A.ty               (* variable *)
  | FuncEntry of A.ty * A.ty list  (* types of params * type of result *)

type 'a table = 'a S.table

let type_mismatch expected actual pos =
  raise (TypeMismatch (expected, actual, pos))

let var_name = function
  | IdentExp (name, pos) -> name
  | _ -> raise (Invalid_argument "not a var!")

let checkComparable ty =
  match ty with
  | (BoolTy | PairTy(_,_) | ArrayTy(_)) -> false
  | _ -> true

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

let is_heap_type  = function
  | A.ArrayTy _ | A.PairTy (_, _) -> true
  | _ -> false

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
      | LitPair -> NullTy
      | Null      -> PairTyy
    end
  | BinOpExp    (exp, binop, exp', pos) -> (
    let ty = exp_type table exp in
    let ty' = exp_type table exp' in
     if eq_type (expect exp table binop pos) ty && eq_type (expect exp table binop pos) ty'
    then binop_type binop
    else
    raise (TypeMismatch ((exp_type table exp), (exp_type table exp'), pos)))
  | UnOpExp     (unop, exp, pos) -> unop_ret_type unop
  | NullExp     (*pos*) -> PairTyy
  | NewPairExp  (exp, exp')(* pos*) -> PairTy ((exp_type table exp), (exp_type table exp'))
  | CallExp     (fname, exps, pos) -> (
    try
      let FuncEntry (retty, argt) = lookup_function table fname in retty (* TODO complete the pattern matching *)
    with
    | Not_found -> raise (UnknownIdentifier (fname, pos)))
  | SndExp  (exp, pos) -> (
      let ty = exp_type' exp in
      match ty with
      | PairTy (t, t') -> t'
      | _ -> raise (TypeMismatch (PairTyy, ty, pos)))
  | FstExp  (exp, pos) -> (
      let ty = exp_type' exp in
      match ty with
      | PairTy (t, t') -> t
      | _ -> raise (TypeMismatch (PairTyy, ty, pos)) )
  | ArrayIndexExp (name, e::exps, pos)-> (
      if ((var_type table name) == StringTy) then CharTy
      else (let ArrayTy ty = var_type table name in
            ty))
  | _ -> failwith "todo exp_type"
and binop_type = function
  | (A.GeOp | A.GtOp | A.LeOp | A.LtOp |
           A.NeOp | A.EqOp | A.AndOp | A.OrOp ) -> A.BoolTy
  | _ -> A.IntTy
and expect exp table binop pos =
   (match binop with
   | (A.OrOp | A.AndOp ) -> A.BoolTy
   | (A.MinusOp | A.ModOp | A.PlusOp | A.TimesOp | A.DivideOp ) -> A.IntTy
   | (A.EqOp | A.NeOp ) -> exp_type table exp
   | _ -> (
    if not(checkComparable (exp_type table exp)) then
    raise (SemanticError(("Unexpected type" ),pos))
    else exp_type table exp;))


and check_function_call table fname exps pos = try
    match S.lookup fname table with
    | FuncEntry (retty, argtys) ->
      begin
        let exp_tys = List.map (exp_type table) exps in
        if argtys = exp_tys then table else (
          raise (SemanticError ("func call type mismatch", pos)))
      end
    | _ -> raise (SemanticError ("not a valid function", pos))
  with
  | Not_found -> raise (UnknownIdentifier (fname, pos))
(* Return type of an expression, will raise TypeMismatch if type mismatch *)
and check_exp (table: 'a S.table) (exp: A.exp): 'a table = (
  let check_in_this_scope = check_exp table in
  match exp with
  | IdentExp    (name, pos) -> (
      try
        let _ = S.lookup name table in table
        with
        | Not_found -> raise (UnknownIdentifier (name, pos)))
  | LiteralExp  (literal, pos) -> table
  | BinOpExp    (exp, binop, exp', pos) -> (
      let lty = exp_type table exp in
      let rty = exp_type table exp' in
      if eq_type (expect exp table binop pos) lty &&  eq_type (expect exp table binop pos) rty then table
      else raise (SemanticError("Unexpected type comparison",pos)))
  | UnOpExp     (unop, exp, pos) -> (
      check_in_this_scope exp;
      let ty = exp_type table exp in
      let expected_ty = (unop_arg_type unop) in
      if eq_type ty expected_ty then table else raise (TypeMismatch (ty, expected_ty, pos)))
  | NullExp     (*pos*) -> table
  | NewPairExp  (exp, exp') (*pos*) -> table
  | CallExp     (symbol, exps, pos) -> check_function_call table symbol exps pos
  | ArrayIndexExp (name, exps, pos) -> table
  | FstExp _ -> table
  | SndExp _ -> table
)
and check_stmt table stmt =
  let check_in_this_scope = check_exp table in
  let table' = S.new_scope table in
  let check_with_new_scope = check_stmt table' in
  match stmt with
  | SeqStmt (RetStmt (_, pos), stmtlist) -> raise (A.SyntaxError ("Junk after return"))
  | SeqStmt (stmt, stmtlist) ->
    (let table' = check_stmt table stmt in
     (check_stmt table' stmtlist))
  | AssignStmt (IdentExp (name, _), exp, pos) -> begin
      try
        if not (is_var table name) then raise (SemanticError ("Not a variable", pos))
        else
          let _ = check_exp table exp in
          let ty = var_type table name in
          let ty' = exp_type table exp in
          if eq_type ty ty' then table else raise (TypeMismatch (ty, ty', pos))
      with Not_found -> raise (SemanticError ("Variable not found", pos))
    end
  | AssignStmt   (lhs, rhs, pos) -> begin
      try
        let _ = check_exp table lhs in
        let _ = check_exp table rhs in
        let ty = exp_type table lhs in
        let ty' = exp_type table rhs in
        if eq_type ty ty' then table else raise (TypeMismatch (ty, ty', pos))
      with Not_found -> raise (SemanticError ("Variable not found", pos))
    end
  | IfStmt       (exp, exp', exp'', pos) -> begin
      ignore(check_in_this_scope exp);
      let ty = exp_type table exp in
      if eq_type ty BoolTy then (ignore(check_with_new_scope exp');
                                 check_with_new_scope exp'')
      else raise (TypeMismatch (BoolTy, ty, pos))
    end
  | WhileStmt    (exp, exp')(*, pos)*) -> begin
      ignore(check_in_this_scope exp);
      let ty = exp_type table exp in
      if eq_type ty BoolTy then check_with_new_scope exp' else raise (SomeError("type mismatch"))(*(TypeMismatch (BoolTy, ty, pos))*)
    end
  | ExitStmt     (exp, pos) -> (if (exp_type table exp) != IntTy then raise (SemanticError ("Exit code is not int", pos)) else table)
  | VarDeclStmt  (ty , symbol, exp, pos) ->
    begin
      let _: 'a table = check_exp table exp in
      let _ = (match S.lookup_opt' symbol table with
        | Some _ as v -> raise (SemanticError ("redeclared variable: " ^ symbol, pos))
        | None -> ()
        ) in
      let table' = S.insert symbol (VarEntry ty) table in
      let ty' = exp_type table' exp in
      if eq_type ty ty' then table' else raise (TypeMismatch (ty, ty', pos))
    end
  | SkipStmt      pos -> table
  (* TODO someof these need to be type check *)
  | PrintStmt    (exp, pos) -> check_in_this_scope exp
  | PrintLnStmt  (exp, pos) -> check_in_this_scope exp
  | RetStmt      (exp, pos) -> (let table'' = check_in_this_scope exp in
                                Symbol.insert "$result" (VarEntry (exp_type table'' exp)) table''
                               )
  | ReadStmt     (exp, pos) -> (check_in_this_scope exp;
                                let is_read_ty = function
                                  | (A.IntTy | A.CharTy) as t -> t
                                  | _ -> raise (SemanticError ("Unsupported read input type", pos)) in
                                is_read_ty (exp_type table exp); table
                               )
  | FreeStmt     (exp, pos) -> (check_in_this_scope exp;
                                let rty = exp_type table exp in
                                if (not (is_heap_type rty)) then
                                  raise (SemanticError ("Can only free heap allocated data", pos))
                                else
                                  table)
  | BlockStmt    (stmt, pos) -> (ignore(check_with_new_scope stmt);
                                 table)
and check_function_decls decls =
  begin
  let ctx = ref Symbol.empty in
  ignore(List.iter (fun x -> (match x with
      | FuncDec (ty, name, args, body, pos) ->
        (try
            let _ = lookup_function !ctx name in
            raise (SemanticError ("Function redefined", pos))
         with
          | Not_found -> ());
        ctx := Symbol.insert name (FuncEntry (ty, List.map (fun (ft, name) -> ft) args)) !ctx
    )) decls);
  let check_dec decl = (match decl with
      | FuncDec (ty, name, args, body, pos) ->
        begin
          let inner_ctx = ref (Symbol.new_scope !ctx) in
          let () = List.iter (fun x -> inner_ctx := (let (t, name) = x in Symbol.insert name (VarEntry t) !inner_ctx)) args in
          ignore(check_stmt !inner_ctx body)
        end) in
  List.map check_dec decls;
  ()
  end

let check_int_overflow num =
  let max_int = Int32.to_int Int32.max_int in
  let min_int = Int32.to_int Int32.min_int in
  (if num <= max_int && num >= min_int
  then num
  else raise (SyntaxError ("Int overflow: " ^ string_of_int num)));;
