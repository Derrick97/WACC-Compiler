open Llvm
module S = Semantic
module A = Ast_v2

exception Error of string;;

let context = global_context ()
let global_module = create_module context "wacc llvm"

let i1_type = i1_type context
let i8_type = i8_type context
let i32_type = i32_type context
let char_type = i8_type
let void_type = void_type context
let fty = function_type i32_type [| |]
let f = define_function "main" fty global_module

open Ast_v2
open TypeKind
let rec lltype_of_ty = function
  | IntTy -> i32_type
  | BoolTy -> i1_type
  | CharTy -> i8_type
  | StringTy -> pointer_type i8_type
  | ArrayTy ty -> pointer_type (lltype_of_ty ty)
  | PairTy (ft, st) -> pointer_type (lltype_of_ty ft)
  | PairTyy -> pointer_type i32_type
  | NullTy -> void_type

let builtin_functions =
  ["wacc_print_bool", i32_type, [|(pointer_type i8_type)|];
   "wacc_print_char", i32_type,  ([|(i8_type)|]);
   "wacc_print_string", i32_type, ([|(pointer_type i8_type)|]);
   "wacc_print_int", i32_type, ([|(i32_type)|]);
   "wacc_print_pair", i32_type, ([|(i8_type)|]);
   "wacc_print_array", i32_type, [|(i8_type)|];
   "wacc_ord", i32_type, [|(i8_type)|];
   "wacc_chr", i8_type, [|(i32_type)|];
   "wacc_read_int", i32_type, [| |];
   "wacc_read_char", i8_type, [| |];
   "wacc_exit", i8_type, [|i32_type|];
  ] |> List.map (fun (fname, retty, argtys) -> begin
        let fsig = function_type retty argtys in
        declare_function fname fsig
      end)

let builder = builder_at_end context (entry_block f)

let rec codegen_printf newline table exp =
  let inst = codegen_exp table exp in
  let exp_ty = type_of inst in
  let ty_str = match (classify_type exp_ty) with
    | Pointer ->
        (* TODO generally handle determining the type of the rhs exp*)
        begin
          match classify_type (element_type exp_ty) with
          | Struct -> "pair"
          | Array -> "array"
          | _ -> "string" (* raise (Error "other format not supported") *)
        end
    | Integer ->
        (match integer_bitwidth exp_ty with
         | 8 -> "char"
         | 32 -> "int"
         | 1 -> "bool"
         | _ -> raise (Error "unknown integer size"))
    | Array -> "string" (*only possible with string since array is malloced *)
    | Struct -> "pair"
    | _ -> raise (Error ("unknown printf for exp"))
  in
  let fname = ("wacc_print_" ^ ty_str) in
  let print_f = match lookup_function fname global_module with
    | Some f -> f
    | None -> raise (Error "function definition not found") in
  let is_ptr v = match classify_type v with
    | Pointer -> true
    | _ -> false
  in
  let inst = (if (is_ptr exp_ty)
              then build_ptrtoint inst i8_type "ptr" builder
              else
              if (String.compare ty_str "bool" = 0) then
                build_intcast inst i8_type "intcast" builder
              else inst)
  in
  build_call print_f (Array.of_list [inst]) fname builder

(* translate the expressions to LLVM IR
 * Pre: the AST has passed semantic analysis, so no typechecking at this phase
*)
and codegen_println table exp = codegen_printf true table exp
and codegen_print table exp = codegen_printf false table exp
and codegen_read (table: Llvm.llvalue Symbol.table) exp = begin
  let exp, pos = exp in
  (* read a valud to the ptr *)
  let read_value ptr =
    let ptr_ty = (type_of ptr) in
    let elem_ty = element_type ptr_ty in (* TODO need some introspection for multidimentional*)
    let width = integer_bitwidth elem_ty in
    if width = 8 then
      let (c) = codegen_func_call table (Symbol.symbol "read_char") [] in
      build_store c ptr builder
    else if width = 32 then
      let (i) = codegen_func_call table (Symbol.symbol "read_int") [] in
      build_store i ptr builder
    else raise (Error "unknown place to read to")
  in
  match exp with
  | IdentExp (name) -> begin
      let ptr = Symbol.lookup name table in
      read_value ptr
    end
  | ArrayIndexExp (name, exps) -> begin
      let var_ptr = Symbol.lookup name table in
      let indices = List.map (codegen_exp table) exps in
      let indices = List.concat [[const_int i32_type 0]; indices] in
      let elem_ptr = build_gep var_ptr (Array.of_list (indices)) "idx" builder in
      read_value elem_ptr
    end
  | FstExp (exp, pos) -> raise (Error "not yet supported")
  | SndExp (exp, pos) -> raise (Error "not yet supported")
  | _ -> raise (Error "Internal error")
end
and codegen_call table fname args = begin
  let f = (match lookup_function fname global_module with
      | Some f -> f
      | None -> invalid_arg "function not found"
    ) in
  build_call f
    (args |> List.map (codegen_exp table) |> Array.of_list)
    fname builder
end
and codegen_exp (table: Llvm.llvalue Symbol.table) (exp: A.exp)
  : (Llvm.llvalue) =
  let exp, pos = exp in
  match exp with
  | ArrayIndexExp(name, exps) -> begin
      let var_ptr = Symbol.lookup name table in
      let indices = List.map (codegen_exp table) exps in
      let indices = List.concat [[const_int i32_type 0]; indices] in (* pass typecheck of llvm IR *)
      let elem_ptr = build_gep var_ptr (Array.of_list (indices)) "idx" builder in
      let v = build_load elem_ptr name builder in
      v
    end
  (* For any identifier, if it is a reference, then
     pass back the pointer. Otherwise the value *)
  | IdentExp (name) -> begin
      let v = Symbol.lookup name table in (* v the the pointer *)
      let var_ty = type_of v in
      let elem_ty = element_type var_ty in
      match classify_type elem_ty with
      | (Pointer | Struct ) ->  v
      | Integer -> build_load v (name) builder
      | _ -> raise (Invalid_argument "Unknown identifier type")
    end
  | LiteralExp (literal) -> begin
      match literal with
      | LitBool b -> const_int i1_type 1
      | LitChar c -> const_int char_type (int_of_char c)
      | LitString str -> build_global_stringptr str "string_literal" builder
      | LitInt  num -> const_int i32_type num
      | LitArray [] -> const_array i32_type (Array.of_list []) (* TODO malloc *)
      | LitArray xs -> begin
          let elts = List.map (codegen_exp table) xs in
          let ty = type_of (List.hd elts) in
          const_array ty (Array.of_list elts)
        end
      | LitPair (f, s) -> begin
          let (fst_inst) = codegen_exp table f in
          let (snd_inst) = codegen_exp table s in
          const_struct context (Array.of_list [fst_inst; snd_inst])
        end
      | LitNull -> const_int i32_type 0
    end
  | BinOpExp (exp, binop, exp') -> begin
      let build_compare cmp lhs rhs =
        match cmp with
        | GeOp ->  build_icmp Icmp.Sge lhs rhs "ge" builder
        | GtOp ->  build_icmp Icmp.Sgt lhs rhs "gt" builder
        | EqOp ->  build_icmp Icmp.Eq lhs rhs  "eq"  builder
        | NeOp ->  build_icmp Icmp.Ne lhs rhs  "ne"  builder
        | LtOp ->  build_icmp Icmp.Slt lhs rhs "lt" builder
        | LeOp ->  build_icmp Icmp.Sle lhs rhs "le" builder
        | _ -> raise (Invalid_argument "not a cmp")
      in
      let lhs' = codegen_exp table exp in
      let rhs' = codegen_exp table exp' in
      match binop with
      | PlusOp ->  build_add lhs' rhs' "add" builder
      | MinusOp -> build_sub lhs' rhs' "sub" builder
      | TimesOp -> build_mul lhs' rhs' "mul" builder
      | DivideOp -> build_sdiv lhs' rhs' "div" builder
      | (GeOp | EqOp | GtOp | LtOp | LeOp | NeOp ) -> build_compare binop lhs' rhs'
      | AndOp -> build_and lhs' rhs' "and" builder
      | OrOp ->  build_or lhs' rhs'  "or" builder
      | ModOp -> build_srem lhs' rhs' "mod" builder
    end
  | UnOpExp (unop, exp) -> begin
      let (subinst) = codegen_exp table exp in
      match unop with
      | NotOp -> (build_not subinst "not" builder)
      | NegOp -> (build_neg subinst "neg" builder)
      | LenOp -> begin
          let ty = type_of subinst in
          let length = array_length ty in
          const_int i32_type length
        end
      | OrdOp -> codegen_call table ("wacc_ord") [exp]
      | ChrOp -> codegen_call table ("wacc_chr") [exp]
      | IncOp -> (invalid_arg "not implemented")
    end
  | NullExp ->  raise (Error "null expression not supported")
  | NewPairExp  (exp, exp') -> begin
      let (fst_inst) = codegen_exp table exp in
      let (snd_inst) = codegen_exp table exp' in
      let struct_ty = struct_type context (Array.of_list [type_of fst_inst; type_of snd_inst]) in
      let v = build_malloc struct_ty "newpair" builder in
      let c = const_struct context (Array.of_list [fst_inst; snd_inst]) in
      ignore(build_store c v builder);
      v
    end
  | CallExp     (fname, exps) -> codegen_call table fname exps
  | FstExp  (exp) -> begin
      let instr = codegen_exp table exp in
      let elem = build_struct_gep instr 0 "fst" builder in
      let inst = build_load elem "load.fst" builder in
      inst
    end
  | SndExp  (exp) ->
      let instr = codegen_exp table exp in
      let elem = build_struct_gep instr 1 "snd" builder in
      let inst = build_load elem "load.snd" builder in
      (inst)
and codegen_decl (FuncDec (ty, name, args, body), _) =
  let func_name = name in
  let ret_ty = lltype_of_ty ty in
  let param_tys = List.map (fun (ty, name) -> lltype_of_ty ty) args in
  let func_ty = function_type ret_ty (Array.of_list param_tys) in
  let f = define_function func_name func_ty global_module in
  let arg_val_pair = Array.mapi (fun i p ->
      let (_, name) = List.nth args i in
      (name, p))(params f) in
  position_at_end (entry_block f) builder;
  let table = Array.fold_left (fun table (name, llval) -> (
        set_value_name (name) llval;
        (* all use the stack *)
        let arg = build_alloca (type_of llval) (name) builder in
        let start_val = build_store llval arg builder in
        Symbol.insert name arg table)) Symbol.empty arg_val_pair in
  ignore (codegen_stmt table body);
  Llvm_analysis.assert_valid_function f;
  f
and codegen_stmt (table: Llvm.llvalue Symbol.table)
    (exp: A.stmt): (Llvm.llvalue Symbol.table) =
  let exp, pos = exp in
  match exp with
  | ExitStmt exp -> begin
      ignore (codegen_call table "wacc_exit" [exp]);
      table
    end
  | RetStmt  exp -> begin
      let exp = codegen_exp table exp in
      ignore(build_ret exp builder);
      table
    end
  | ReadStmt ((A.IdentExp (_), _) as exp) -> ignore (codegen_read table exp);  table
  | ReadStmt (_, _) -> (invalid_arg "Cannot read to anything other than var")
  | FreeStmt exp -> begin
      let inst = codegen_exp table exp in
      ignore(build_free inst builder); table
    end
  | AssignStmt   ((IdentExp (name), _), rhs) -> begin
      let instr = codegen_exp table rhs in
      let assign = Symbol.lookup name table in
      ignore (build_store instr assign builder );
      table
    end
  | AssignStmt ((ArrayIndexExp (name, idxs), _) , rhs) -> begin
      let array_ptr = Symbol.lookup name table in
      let indices = (List.map (codegen_exp table) idxs) in
      let indices = List.concat [[const_int i32_type 0]; indices] in
      let elem_ptr = build_gep array_ptr (Array.of_list (indices)) "idx" builder in
      let instr = codegen_exp table rhs in
      ignore ( build_store instr elem_ptr builder );
      (table)
    end
  | AssignStmt ((FstExp (IdentExp lhs, _), _), rhs) -> begin
      let instr = codegen_exp table rhs in
      let pair = Symbol.lookup lhs table in
      let elem = build_struct_gep pair 0 "fst" builder in
      ignore( build_store instr elem builder );
      (* Update the SSA in symbol table *)
      (table)
    end
  | AssignStmt ((SndExp (IdentExp lhs, _), _), rhs) -> begin
      let instr = codegen_exp table rhs in
      let pair = Symbol.lookup lhs table in
      let elem = build_struct_gep pair 1 "snd" builder in
      ignore (build_store instr elem builder );
      table
    end
  | AssignStmt (_, _) -> (invalid_arg "assignment of non-identity not supported")
  (* Declare an array from array literal*)
  | VarDeclStmt   (ArrayTy ty, name, exp) -> begin
      let start_val = codegen_exp table exp in
      let array_ty = type_of start_val in
      let len = array_length array_ty in
      let array_ty = array_type (element_type array_ty) len in (* first block stores the length *)
      let array_ptr = build_malloc array_ty name builder in
      (* print_string (string_of_llvalue array_ptr); *)
      let table = Symbol.insert name array_ptr table in
      (* store needs to store both the length and the items *)
      ignore(build_store start_val array_ptr builder);
      (* insert the new variable symbol into our table. *)
      table
    end
  | VarDeclStmt (PairTy (fst_ty, snd_ty), name, exp) -> begin
      let (pair_ptr) = codegen_exp table exp in
      let table = Symbol.insert name pair_ptr table in
      table
    end
  | VarDeclStmt   (ty, name, exp) -> begin
      let start_val = codegen_exp table exp in
      let exp_type = type_of start_val in
      let var_ptr = build_alloca exp_type name builder in
      let table = Symbol.insert name var_ptr table in
      ignore(build_store start_val var_ptr builder);
      (* insert the new variable symbol into our table. *)
      table
    end
  | SkipStmt -> table
  | PrintStmt (newline, exp) -> ignore (codegen_print table exp); table
  | WhileStmt (pred, body) -> begin
      let start_bb = insertion_block builder in
      (* where we want to insert *)
      let parent = block_parent start_bb in
      (* block for checking the loop conditional *)
      let loop_cond_bb = append_block context "loop.cond" parent in
      let loop_bb = append_block context "loop.body" parent in (* block for the loop *)
      let loop_end_bb = append_block context "loop.done" parent in (* block for when loop terminates *)
      (* a fall through *)
      ignore (build_br loop_cond_bb builder);
      position_at_end loop_cond_bb builder;
      let (pred) = codegen_exp table pred in
      ignore (build_cond_br pred loop_bb loop_end_bb builder);
      (* Body of the while loop *)
      position_at_end loop_bb builder;
      let loop_ret = build_br loop_cond_bb builder in
      ignore (position_before loop_ret builder);
      (* Escape the for loop *)
      position_at_end loop_end_bb builder;
      table
    end
  | BlockStmt (body) -> begin
      let table = Symbol.new_scope table in
      codegen_stmt table body
    end
  | SeqStmt (stmt, stmtlist) -> begin
      let table' = codegen_stmt table stmt in
      codegen_stmt table' stmtlist
    end
  | IfStmt (pred, then_, else_) -> begin
      (* See Kaleidoscope for explanation of the implementation *)
      let pred = codegen_exp table pred in
      let one = const_int i1_type 1 in
      let cond_val = build_icmp Llvm.Icmp.Eq pred one "if.cond" builder in
      let start_bb = insertion_block builder in
      let the_func = block_parent start_bb in
      let then_bb = append_block context "if.then" the_func in
      position_at_end then_bb builder;
      let table = codegen_stmt table then_ in
      let new_then_bb = insertion_block builder in
      let else_bb = append_block context "if.else" the_func in
      position_at_end else_bb builder;
      let table = codegen_stmt table else_ in
      let new_else_bb = insertion_block builder in
      let merge_bb = append_block context "if.cont" the_func in
      position_at_end merge_bb builder;
      position_at_end start_bb builder;
      ignore (build_cond_br cond_val then_bb else_bb builder);
      position_at_end new_then_bb builder; ignore (build_br merge_bb builder);
      position_at_end new_else_bb builder; ignore (build_br merge_bb builder);
      position_at_end merge_bb builder;
      (table)
    end
  | _ -> failwith "TODO"

and add_function_decs decs env =
  let env = ref env in
  let add_function_def f = begin
    let (A.FuncDec (retty, ident, fields, body), _) = f in
    let argtys = List.map fst fields in
     env := Symbol.insert ident (Env.FuncEntry (retty, argtys)) !env
  end in
  List.iter add_function_def decs;
  !env

let trans_prog ctx (decs, stmt) (out: out_channel): unit = begin
  (* let ctx = add_function_decs decs ctx in *)
  ignore(codegen_stmt ctx stmt);
  ignore(build_ret (const_int i32_type 0) builder);
  output_string out (Llvm.string_of_llmodule global_module);
end
