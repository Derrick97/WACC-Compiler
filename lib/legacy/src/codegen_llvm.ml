open Llvm
module S = Semantic

exception Error of string;;

let context = global_context ()
let the_module = create_module context "wacc llvm"
let builder = builder context
let i1_type = i1_type context
let i8_type = i8_type context
let i32_type = i32_type context
let char_type = i8_type
let void_type = void_type context

module Llvm_backend = struct
  open Ast
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

  let setup_builtin_functions =
    (* setup printf *)
    let print_fty = var_arg_function_type i8_type (Array.of_list  [(pointer_type i8_type)]) in
    let _ = declare_function "printf" print_fty the_module in

    let print_bool_fty = function_type i32_type (Array.of_list [(i8_type)]) in
    let _ = declare_function "print_bool" print_bool_fty the_module in
    let _ = declare_function "println_bool" print_bool_fty the_module in

    let print_char_fty = function_type i32_type (Array.of_list [(i8_type)]) in
    let _ = declare_function "print_char" print_char_fty the_module in
    let _ = declare_function "println_char" print_char_fty the_module in

    let print_string_fty = function_type i32_type (Array.of_list [(pointer_type i8_type)]) in
    let _ = declare_function "print_string" print_string_fty the_module in
    let _ = declare_function "println_string" print_string_fty the_module in

    let print_int_fty = function_type i32_type (Array.of_list [(i32_type)]) in
    let _ = declare_function "print_int" print_int_fty the_module in
    let _ = declare_function "println_int" print_int_fty the_module in

    let print_pair_fty = function_type i32_type (Array.of_list [(i8_type)]) in
    let _ = declare_function "print_pair" print_pair_fty the_module in
    let _ = declare_function "println_pair" print_pair_fty the_module in

    let print_array_fty = function_type i32_type (Array.of_list [(i8_type)]) in
    let _ = declare_function "print_array" print_array_fty the_module in
    let _ = declare_function "println_array" print_array_fty the_module in

    (* ord, chr *)
    let ord_fty = function_type i32_type (Array.of_list [i8_type]) in
    let _ = declare_function "ord" ord_fty the_module in
    let chr_fty = function_type i8_type (Array.of_list [i32_type]) in
    let _ = declare_function "chr" chr_fty the_module in
    (* len *)
    (* read, including read_int read_char separately *)
    let read_int_fty = function_type i32_type (Array.of_list []) in
    let _ = declare_function "read_int" read_int_fty the_module in
    let read_char_fty = function_type i8_type (Array.of_list []) in
    let _ = declare_function "read_char" read_char_fty the_module in
    ()

  let rec codegen_printf newline table exp =
    let (_, inst) = codegen_exp table exp in
    let exp_ty = type_of inst in
    let print_println = (if (newline) then "println_" else "print_")
    in
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
    let fname = (print_println ^ ty_str) in
    let print_f = match lookup_function fname the_module with
      | Some f -> f
      | None -> raise (Error "function definition not found") in
    let is_ptr v = match classify_type v with
      | Pointer -> true
      | _ -> false
    in
    let ptr = (if (is_ptr exp_ty)
      then build_ptrtoint inst i8_type "ptr" builder
      else
        if (String.compare ty_str "bool" = 0)
        then
          build_intcast inst i8_type "intcast" builder
        else inst )
    in
    build_call print_f (Array.of_list [inst]) fname builder

  (* translate the expressions to LLVM IR
   * Pre: the AST has passed semantic analysis, so no typechecking at this phase
  *)
  and codegen_println table exp = codegen_printf true table exp
  and codegen_print table exp = codegen_printf false table exp
  and codegen_read (table: Llvm.llvalue Symbol.table) exp =
    begin
      (* read a valud to the ptr *)
      let read_value ptr =
        let ptr_ty = (type_of ptr) in
        let elem_ty = element_type ptr_ty in (* TODO need some introspection for multidimentional*)
        let width = integer_bitwidth elem_ty in
        if width = 8 then
          let (_, c) = codegen_func_call table (Symbol.symbol "read_char") [] in
          table, build_store c ptr builder
        else if width = 32 then
          let (_, i) = codegen_func_call table (Symbol.symbol "read_int") [] in
          table, build_store i ptr builder
        else raise (Error "unknown place to read to")
      in
      match exp with
      | IdentExp (name, pos) ->
        begin
          let ptr = Symbol.lookup name table in
          read_value ptr
        end
      | ArrayIndexExp (name, exps, pos) ->
        begin
          let var_ptr = Symbol.lookup name table in
          let indices = (List.map (fun (_, elt) -> elt) (List.map (codegen_exp table) exps)) in
          let indices = List.concat [[const_int i32_type 0]; indices] in
          let elem_ptr = build_gep var_ptr (Array.of_list (indices)) "idx" builder in
          read_value elem_ptr
        end
      | FstExp (exp, pos) -> raise (Error "not yet supported")
      | SndExp (exp, pos) -> raise (Error "not yet supported")
      | _ -> raise (Error "Internal error")
    end
  and codegen_exp (table: Llvm.llvalue Symbol.table) (exp: Ast.exp)
      : (Llvm.llvalue Symbol.table * Llvm.llvalue) =
    match exp with
    | ArrayIndexExp(name, exps, pos) ->
      begin
        let var_ptr = Symbol.lookup name table in
        let indices = (List.map (fun (_, elt) -> elt) (List.map (codegen_exp table) exps)) in
        let indices = List.concat [[const_int i32_type 0]; indices] in (* pass typecheck of llvm IR *)
        let elem_ptr = build_gep var_ptr (Array.of_list (indices)) "idx" builder in
        let v = build_load elem_ptr (Symbol.name name) builder in
        table, v
      end
    (* For any identifier, if it is a reference, then
       pass back the pointer. Otherwise the value *)
    | IdentExp (name, pos) ->
      begin
        let v = Symbol.lookup name table in (* v the the pointer *)
        let var_ty = type_of v in
        let elem_ty = element_type var_ty in
        match classify_type elem_ty with
        | (Pointer | Struct ) ->  table, v
        | Integer -> table, build_load v (Symbol.name name) builder
        | _ -> raise (Invalid_argument "Unknown identifier type")
      end
    | LiteralExp (literal, pos) ->
      begin
        match literal with
        | LitBool b -> table, const_int i1_type 1
        | LitChar c -> table, const_int char_type (int_of_char c)
        | LitString str -> table, build_global_stringptr str "string_literal" builder
        | LitInt  num -> table, const_int i32_type num
        | LitArray [] ->
          table, const_array i32_type (Array.of_list []) (* TODO malloc *)
        | LitArray xs -> begin
            let elts = (List.map (fun (_, elt) -> elt) (List.map (codegen_exp table) xs)) in
            let ty = type_of (List.hd elts) in
            table, const_array ty (Array.of_list elts)
          end
        | LitPair (f, s) -> begin
            let (_, fst_inst) = codegen_exp table f in
            let (_, snd_inst) = codegen_exp table s in
            let c = const_struct context (Array.of_list [fst_inst; snd_inst]) in
            table, c
          end
        | Null -> table, const_int i32_type 0
      end
    | BinOpExp (exp, binop, exp', pos) ->
      begin
        let build_compare cmp lhs rhs =
          match cmp with
          | GeOp ->  table, build_icmp Icmp.Sge lhs rhs "ge" builder
          | GtOp ->  table, build_icmp Icmp.Sgt lhs rhs "gt" builder
          | EqOp ->  table, (build_icmp Icmp.Eq lhs rhs "eq" builder)
          | NeOp ->  table, build_icmp Icmp.Ne lhs rhs "ne" builder
          | LtOp ->  table, build_icmp Icmp.Slt lhs rhs "lt" builder
          | LeOp ->  table, build_icmp Icmp.Sle lhs rhs "le" builder
          | _ -> raise (Invalid_argument "not a cmp")
        in
        let (_, lhs') = codegen_exp table exp in
        let (_, rhs') = codegen_exp table exp' in
        begin
          match binop with
          | PlusOp ->  table, build_add lhs' rhs' "add" builder
          | MinusOp -> table, build_sub lhs' rhs' "sub" builder
          | TimesOp -> table, build_mul lhs' rhs' "mul" builder
          | DivideOp -> table, build_sdiv lhs' rhs' "div" builder
          | (GeOp | EqOp | GtOp | LtOp | LeOp | NeOp ) -> build_compare binop lhs' rhs'
          | AndOp -> table, build_and lhs' rhs' "and" builder
          | OrOp ->  table, build_or lhs' rhs' "or" builder
          | ModOp -> table, build_srem lhs' rhs' "mod" builder
        end
      end
    | UnOpExp (unop, exp, pos) ->
      begin
        let (_, subinst) = codegen_exp table exp in
        match unop with
        | NotOp -> (table, build_not subinst "not" builder)
        | NegOp -> (table, build_neg subinst "neg" builder)
        | LenOp ->
          begin
            let ty = type_of subinst in
            let length = array_length ty in
            table, const_int i32_type length
          end
        | OrdOp -> codegen_func_call table (Symbol.symbol "ord") [exp]
        | ChrOp -> codegen_func_call table (Symbol.symbol "chr") [exp]
      end
    | NullExp _ ->  raise (Error "null expression not supported")
    | NewPairExp  (exp, exp', pos) ->
      begin
        let (_, fst_inst) = codegen_exp table exp in
        let (_, snd_inst) = codegen_exp table exp' in
        let struct_ty = struct_type context (Array.of_list [type_of fst_inst; type_of snd_inst]) in
        let v = build_malloc struct_ty "newpair" builder in
        let c = const_struct context (Array.of_list [fst_inst; snd_inst]) in
        ignore(build_store c v builder);
        table, v
      end
    | CallExp     (fname, exps, pos) ->
      begin
        let fname = Symbol.name fname in
        let f = (match lookup_function fname the_module with
            | Some f -> f
            | None -> raise (Error "function not found")
          ) in
        table, build_call f (Array.of_list (List.map (fun exp ->
            let (_, ins) = codegen_exp table exp in ins) exps)) fname builder
      end
    | FstExp  (exp, pos) ->
      let (table, instr) = codegen_exp table exp in
      let elem = build_struct_gep instr 0 "fst" builder in
      let inst = build_load elem "load.fst" builder in
      (table, inst)
    | SndExp  (exp, pos) ->
      let (table, instr) = codegen_exp table exp in
      let elem = build_struct_gep instr 1 "snd" builder in
      (* let elem = build_gep instr (Array.of_list [const_int i32_type 1]) "snd" builder in *)
      let inst = build_load elem "load.snd" builder in
      (table, inst)
  and codegen_func_call table fname exps =
    let fname = Symbol.name fname in
    let f = (match lookup_function fname the_module with
        | Some f -> f
        | None -> raise (Error "function not found")
      ) in
    table, build_call f (Array.of_list (List.map (fun exp ->
        let (_, ins) = codegen_exp table exp in ins) exps)) fname builder
  and codegen_decl (FuncDec (ty, name, args, body, _)) =
    let func_name = Symbol.name name in
    let ret_ty = lltype_of_ty ty in
    let param_tys = List.map (fun (ty, name) -> lltype_of_ty ty) args in
    let func_ty = function_type ret_ty (Array.of_list param_tys) in
    let f = define_function func_name func_ty the_module in
    let arg_val_pair = Array.mapi (fun i p ->
        let (_, name) = List.nth args i in
        (name, p))(params f) in
    position_at_end (entry_block f) builder;
    let table = Array.fold_left (fun table (name, llval) -> (
          set_value_name (Symbol.name name) llval;
          (* all use the stack *)
          let arg = build_alloca (type_of llval) (Symbol.name name) builder in
          let start_val = build_store llval arg builder in
          Symbol.insert name arg table)) Symbol.empty arg_val_pair in
    let _ = codegen_stmt table body in
    Llvm_analysis.assert_valid_function f;
    f
  and codegen_stmt (table: Llvm.llvalue Symbol.table) (exp: Ast.stmt)
    : (Llvm.llvalue Symbol.table * Llvm.llvalue) =
    match exp with
    | ExitStmt (exp, pos) -> table, build_ret_void builder
    | RetStmt  (exp, pos) -> (let (_, exp') = codegen_exp table exp in
                              table, build_ret exp' builder)
    | ReadStmt (IdentExp (var, p), _) -> codegen_read table (IdentExp (var, p))
    | ReadStmt (_, _) -> raise (Error "Cannot read to anything other than var")
    | FreeStmt (exp, pos) ->
      begin
        let (_, inst) = codegen_exp table exp in
        table, build_free inst builder
      end
    | AssignStmt   (IdentExp (name, _), rhs, _) ->
      begin
        let (table, instr) = codegen_exp table rhs in
        let assign = Symbol.lookup name table in
        let assigned = build_store instr assign builder in
        (* Update the SSA in symbol table *)
        (table, assigned)
      end
    | AssignStmt (ArrayIndexExp (name, idxs, _) , rhs, _) ->
      begin
        let array_ptr = Symbol.lookup name table in
        let indices = (List.map (fun (_, elt) -> elt) (List.map (codegen_exp table) idxs)) in
        let indices = List.concat [[const_int i32_type 0]; indices] in
        let elem_ptr = build_gep array_ptr (Array.of_list (indices)) "idx" builder in
        let (table, instr) = codegen_exp table rhs in
        let assigned = build_store instr elem_ptr builder in
        (* Update the SSA in symbol table *)
        (table, assigned)
      end
    | AssignStmt (FstExp (IdentExp (lhs, pos), _), rhs, _) ->
      begin
        let (table, instr) = codegen_exp table rhs in
        let pair = Symbol.lookup lhs table in
        let elem = build_struct_gep pair 0 "fst" builder in
        let assigned = build_store instr elem builder in
        (* Update the SSA in symbol table *)
        (table, assigned)
      end
    | AssignStmt (SndExp (IdentExp (lhs, pos), _), rhs, _) ->
      begin
        let (table, instr) = codegen_exp table rhs in
        let pair = Symbol.lookup lhs table in
        let elem = build_struct_gep pair 1 "snd" builder in
        let assigned = build_store instr elem builder in
        (* Update the SSA in symbol table *)
        (table, assigned)
      end
    | AssignStmt (_, _, _) -> raise (Error "assignment of non-identity not allowed")
    (* Declare an array from array literal*)
    | VarDeclStmt   (ArrayTy ty, name, exp, pos) ->
      begin
        let (_, start_val) = codegen_exp table exp in
        let array_ty = type_of start_val in
        let len = array_length array_ty in
        let array_ty = array_type (element_type array_ty) len in (* first block stores the length *)
        let array_ptr = build_malloc array_ty (Symbol.name name) builder in
        (* print_string (string_of_llvalue array_ptr); *)
        let table = Symbol.insert name array_ptr table in
        (* store needs to store both the length and the items *)
        ignore(build_store start_val array_ptr builder);
        (* insert the new variable symbol into our table. *)
        table, array_ptr
      end
    | VarDeclStmt (PairTy (fst_ty, snd_ty), name, exp, pos) ->
      begin
        let (table, pair_ptr) = codegen_exp table exp in
        let table = Symbol.insert name pair_ptr table in
        table, pair_ptr
      end
    | VarDeclStmt   (ty, name, exp, pos) ->
      begin
        let (_, start_val) = codegen_exp table exp in
        let exp_type = type_of start_val in
        let var_ptr = build_alloca exp_type (Symbol.name name) builder in
        let table = Symbol.insert name var_ptr table in
        ignore(build_store start_val var_ptr builder);
        (* insert the new variable symbol into our table. *)
        table, start_val
      end
    | SkipStmt _ -> table, const_string context "skip" (* dummy for type coherence *)
    | PrintStmt (exp, _) -> table, codegen_print table exp
    | PrintLnStmt (exp, _) -> table, codegen_println table exp
    | WhileStmt (pred, body, _) ->
      begin
        let start_bb = insertion_block builder in
        let parent = block_parent start_bb in (* where we want to insert *)
        let loop_cond_bb = append_block context "loop.cond" parent in (* block for checking the loop conditional *)
        let loop_bb = append_block context "loop.body" parent in (* block for the loop *)
        let loop_end_bb = append_block context "loop.done" parent in (* block for when loop terminates *)
        (* a fall through *)
        ignore (build_br loop_cond_bb builder);
        position_at_end loop_cond_bb builder;
        let (_, pred) = codegen_exp table pred in
        ignore (build_cond_br pred loop_bb loop_end_bb builder);
        (* Body of the while loop *)
        position_at_end loop_bb builder;
        let loop_ret = build_br loop_cond_bb builder in
        ignore (position_before loop_ret builder);
        let (_, body_instr) = codegen_stmt table body in
        (* Escape the for loop *)
        position_at_end loop_end_bb builder;
        table, body_instr
      end
    | BlockStmt (body, _) -> (let table = Symbol.new_scope table in codegen_stmt table body)
    | SeqStmt ([])   ->
      begin
        let bb = insertion_block builder in
        let bb_term = (match block_terminator bb with
            | Some b -> b
            | None -> build_ret (const_int i1_type 1) builder) in
        table, bb_term
      end
    | SeqStmt (x::xs) -> (let (table, _) = codegen_stmt table x in
                          codegen_stmt table (SeqStmt xs))
    | IfStmt (pred, then_, else_, _) ->
      begin
        (* See Kaleidoscope for explanation of the implementation *)
        let (table, pred) = codegen_exp table pred in
        let one = const_int i1_type 1 in
        let cond_val = build_icmp Llvm.Icmp.Eq pred one "if.cond" builder in
        let start_bb = insertion_block builder in
        let the_func = block_parent start_bb in
        let then_bb = append_block context "if.then" the_func in
        position_at_end then_bb builder;
        let (table, then_val) = codegen_stmt table then_ in
        let new_then_bb = insertion_block builder in
        let else_bb = append_block context "if.else" the_func in
        position_at_end else_bb builder;
        let (table, else_val) = codegen_stmt table else_ in
        let new_else_bb = insertion_block builder in
        let merge_bb = append_block context "if.cont" the_func in
        position_at_end merge_bb builder;
        let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
        let phi = build_phi incoming "if.phi" builder in
        position_at_end start_bb builder;
        ignore (build_cond_br cond_val then_bb else_bb builder);
        position_at_end new_then_bb builder; ignore (build_br merge_bb builder);
        position_at_end new_else_bb builder; ignore (build_br merge_bb builder);
        position_at_end merge_bb builder;
        (table, phi)
      end
end
