module A = Ast;;
module E = Env;;
module S = Semantic;;
module F = Arm;;
open Env;;

type frag = string list
type size = int

type stmt = Arm.inst'
type temp = Temp.temp

type access = Env.access

type frame = {
  mutable frame_counter: int;
  mutable frame_offset: int;
  mutable frame_locals: access array;
}

let counter = ref 0

let new_label ?(prefix="L") (): string =
  let i = !counter in
  counter := !counter + 1;
  prefix ^ (string_of_int i)

let new_namedlabel name = name

let strings: (string * string) list ref = ref []

let new_frame frame = {
  frame_counter = 0;
  frame_offset = 0;
  frame_locals = [| |];
}


let allocate_local (frame: frame) (size: size) =
  let offset = frame.frame_offset in
  let a = InFrame (offset, size) in
  frame.frame_offset <- offset + size;
  frame.frame_locals <- Array.append frame.frame_locals [|a|];
  a


let size_of_type = function
  | A.CharTy | A.BoolTy -> 1
  | A.IntTy -> 4
  | A.StringTy -> 4             (* FIXME *)
  | A.PairTy _ -> 4
  | A.NullTy -> 4
  | A.PairTyy -> 4
  | _ -> assert false

let trans_call
    (fname: string)
    (args: temp list): (stmt list) = begin
  (* FIXME transfer the result back to some register *)
  let fname_label = new_namedlabel fname in
  let ilist = ref [] in
  let emit x = ilist := !ilist @ [x] in
  let rec zip a b = match (a, b) with
    | ([], _) -> []
    | (_, []) -> []
    | (x::xs, y::ys) -> (x, y)::(zip xs ys) in
  assert (List.length args <= 3); (* FIXME we only handle arguments less than 3 for now *)
  List.iter (fun (a, b) ->
      emit(F.mov a (Arm.OperReg (b, None)))) (zip F.caller_saved_regs args);
  !ilist @ [F.bl fname_label]
end

let trans_ifelse (cond: temp) (t: stmt list) (f: stmt list) = begin
  let open F in
  let true_l = new_label ~prefix:"if_then" () in
  let false_l = new_label ~prefix:"if_else" () in
  let end_l = new_label ~prefix:"if_end" () in
  [cmp cond (Arm.OperImm 1);
   jump ~cond:Arm.NE false_l;
   labels true_l] @ t @
  [jump end_l] @
  [labels false_l] @ f @
  [labels end_l]
end

let trans_var (var: access) (t: temp): (stmt list) =
  match var with
  | InFrame (offset, sz) ->
    (match sz with
     | 4 -> [F.load t (Arm.AddrIndirect  (Arm.reg_SP, offset))]
     | 1 -> [F.loadb t (Arm.AddrIndirect (Arm.reg_SP, offset))]
     | _ -> assert false)
  | InReg (reg_num) -> [F.mov t (F.OperReg(reg_num,None))]

let trans_assign
    (lv: access)
    (rv: temp): (stmt list) = begin
  match lv with
  | InFrame (offset, sz) -> (
    match sz with
    | 4 -> [F.str rv (Arm.AddrIndirect  (Arm.reg_SP, offset))]
    | 1 -> [F.strb rv (Arm.AddrIndirect (Arm.reg_SP, offset))]
    | _ -> assert false)
  | InReg (reg_num) -> [F.mov rv (F.OperReg(reg_num,None))]
end

let trans_noop: stmt list = []

let rec translate_exp
    (env: E.env)
    (exp: A.exp)
    (regs: temp list): (Arm.inst' list) =
  let tr = translate_exp env in
  let open Arm in
  let check_overflow_inst = bl ~cond:VS "wacc_throw_overflow_error" in
  (match regs with
   | (dst::rest) -> begin
       match exp with
       | A.IdentExp    (name, pos) -> begin
           let E.VarEntry (t, Some acc) = Symbol.lookup name env in
	   let is_pair = function
	    | A.PairTy _ | A.PairTyy -> true
	    | _ -> false in
	   if is_pair t then
	     trans_var acc dst
	   else
             trans_var acc dst
         end
       | A.LiteralExp  (literal, pos) -> begin match literal with
           (* NOTE for int literal, we use load instead of move
              to handle large literals *)
           | A.LitInt i -> [load dst (Arm.AddrLabel (string_of_int i))]
           | A.LitString s -> begin
               let label = new_label() in
               strings := (label, s)::!strings;
               [load dst (AddrLabel label)]
             end
           | A.LitBool b -> if b then
               [mov dst (Arm.OperImm 1)]
             else
               [mov dst (Arm.OperImm 0)]
           | A.LitChar c -> [mov dst (Arm.OperChar c)]
           |  _ -> assert false
         end
       | A.BinOpExp    (exp, binop, exp', pos) -> begin
           let open Arm in
           let (dst::next::rest) = regs in
           let lhs = (tr exp  (dst::next::rest)) in
           let rhs = (tr exp' (next::rest)) in
           let oper = OperReg (next, None) in
           lhs @ rhs @
           (match binop with
            | A.PlusOp ->  [add dst dst oper;
                            check_overflow_inst]
            | A.MinusOp -> [sub dst dst oper;
                            check_overflow_inst]
            | A.TimesOp -> [smull dst next dst next;
                            cmp next (OperReg (dst, Some (ASR 31)));
                            bl ~cond:NE "wacc_throw_overflow_error";
                            (* TODO intmultoverflow handle correctly *)
                            (* https://community.arm.com/processors/b/blog/posts/detecting-overflow-from-mul *)
                            check_overflow_inst]
            | A.DivideOp -> trans_call "wacc_div" [dst;next] @ [mov dst (OperReg (F.reg_RV, None))]
            | A.AndOp -> [annd dst dst oper]
            | A.OrOp  -> [orr dst dst oper]
            | A.ModOp -> begin
                trans_call "wacc_mod" [dst;next] @ [mov dst (OperReg (F.reg_RV, None))]
              end
            | A.GeOp -> begin  (* FIXME we can use table driven methods here *)
                [cmp dst oper;
                 mov ~cond:GE dst (OperImm 1);
                 mov ~cond:LT dst (OperImm 0)]
              end
            | A.GtOp -> begin
                [cmp dst oper;
                 mov ~cond:GT dst (OperImm 1);
                 mov ~cond:LE dst (OperImm 0)]
              end
            | A.LeOp -> begin
                [cmp dst oper;
                 mov ~cond:LE dst (OperImm 1);
                 mov ~cond:GT dst (OperImm 0)]
              end
            | A.LtOp -> begin
                [cmp dst oper;
                 mov ~cond:LT dst (OperImm 1);
                 mov ~cond:GE dst (OperImm 0)]
              end
            | A.EqOp -> begin
                [cmp dst oper;
                 mov ~cond:EQ dst (OperImm 1);
                 mov ~cond:NE dst (OperImm 0)]
              end
            | A.NeOp -> begin
                [cmp dst oper;
                 mov ~cond:NE dst (OperImm 1);
                 mov ~cond:EQ dst (OperImm 0)]
              end)
         end
       | A.UnOpExp     (unop, exp, pos) -> begin
           let ri = (translate_exp env exp regs) in
           let (fst_rest::others) = rest in
           ri @ (match unop with
               | A.NotOp -> [eor dst dst (OperImm 1)]
               | A.NegOp -> begin
                   [mov fst_rest (OperImm 0);
                    sub dst fst_rest (OperReg (dst, None));
                    check_overflow_inst]
                 end
               | A.LenOp -> trans_call "wacc_len" [dst] @ [mov dst (OperReg (reg_RV, None))]
               | A.OrdOp -> trans_call "wacc_ord" [dst]
               | A.ChrOp -> trans_call "wacc_chr" [dst])
         end
       | A.ArrayIndexExp (name, [exp], _) -> begin
           let open Arm in
           let E.VarEntry (t, Some acc) = Symbol.lookup name env in
           let size = 4 in
           let index::addr::o::rest = rest in
           tr exp (index::rest)
           @ trans_var acc addr
           @ trans_call "wacc_check_array_bounds" [addr; index]
           @ [mov o (OperImm size);
              mul index index o;
              add index index (OperImm 4);
              add addr addr (OperReg (index, None));
              load dst (AddrIndirect (addr, 0)) ]
         end
       | A.ArrayIndexExp (_, _, _)
         -> failwith "Only single dimensional array access is supported"
       | A.FstExp (exp, _) -> begin
           let (dst::next::rest) = regs in
           let insts = tr exp (regs) in
           insts @ trans_call "wacc_check_pair_null" [dst] @
           [load next (AddrIndirect(dst, 0));
            load dst (AddrIndirect(next, 0))]
           end
       | A.SndExp (exp, _) -> begin
           let (dst::next::rest) = regs in
           let insts = tr exp (regs) in (* dst stores address to pair *)
           insts @ trans_call "wacc_check_pair_null" [dst] @
           [load next (AddrIndirect(dst, 4)); (* this get address of the second element *)
            load dst (AddrIndirect(next, 0))] (* now we load it *)
         end
       | A.CallExp (sym, exp_list, _) -> begin
           if List.length exp_list > 0 then
           process_function_arguments sym exp_list caller_saved_regs [] env @ [mov dst (OperReg(reg_RV, None))]
           else
           process_function_arguments sym exp_list caller_saved_regs [] env
        end
     end
   | [] -> invalid_arg "Registers have run out")

   and process_function_arguments sym exp_list regs used_reg env =
      let open Arm in
      let (dst::rest) = regs in
      match exp_list with
        | [] -> begin
          trans_call ("f_" ^ sym) used_reg
          (*@ [add reg_SP reg_SP (OperImm(offset)); mov last_reg (OperReg(reg_RV, None))]*)
          end
        | h::r -> begin
            (*let exp_ty = Semantic.check_exp env h in
            let size_offset =  size_of_type exp_ty in
            let save_para_inst = if size_offset = 4 then [str dst (AddrIndirect(reg_SP, -size_offset))] else [strb dst (AddrIndirect(reg_SP, -size_offset))] in
            translate_exp env h [dst] @
            save_para_inst @ [sub reg_SP reg_SP (OperImm(size_offset))] @
            process_function_arguments sym r rest (dst::used_reg) env (offset+size_offset)*)
            translate_exp env h [dst]  @ process_function_arguments sym r rest (dst::used_reg) env
            end

and translate (env: E.env)
    (frame: frame)
    (regs: temp list) (stmt: A.stmt): (Arm.inst' list * E.env) =
  let open Ast in
  let open Arm in
  let tr x = (translate_exp env x regs) in
  let dst::rest = regs in
  let addr_of_exp (e: A.exp) (regss) = match e with
    | ArrayIndexExp (name, [exp], _) -> begin
        let open Arm in
        let E.VarEntry (t, Some acc) = Symbol.lookup name env in
        let size = 4 in
        let index::addr::o::rest = regss in
        let insts = translate_exp env exp (index::rest)
           @ trans_var acc addr @ trans_call "wacc_check_array_bounds" [addr; index]
           @ [mov o (OperImm size);
              mul index index o;
              add index index (OperImm 4);
              add addr addr (OperReg (index, None));] in
        AddrIndirect (addr, 0), insts
      end
    | ArrayIndexExp _ -> assert false
    | IdentExp (name, _) -> begin
        let VarEntry (ty, Some (InFrame (offset, sz) as acc)) = (Symbol.lookup name env) in
	let is_pair = function
	  | A.PairTy _ | A.PairTyy -> true
	  | _ -> false in
	let insts = trans_var acc dst in
        AddrIndirect (Arm.reg_SP, offset), insts
      end
    | FstExp (exp, _) -> begin
        let dst::next::rest = regss in
        let insts = translate_exp env exp regss @ trans_call "wacc_check_pair_null" [dst] in
        AddrIndirect (next, 0), insts @ [load next (AddrIndirect (dst, 0))]
      end
    | SndExp (exp, _) -> begin
        let dst::next::rest = regss in
        let insts = translate_exp env exp regss @ trans_call "wacc_check_pair_null" [dst] in
        AddrIndirect (next, 0), insts @ [load next (AddrIndirect (dst, 4))]
      end
    | _ -> invalid_arg "Not an lvalue" in
  match stmt with
  | SeqStmt (stmt, stmtlist) -> begin
      let exp,  env' = translate env frame regs stmt in
      let exp', env'' = (translate env' frame regs stmtlist) in
      (exp @ exp', env'')
    end
  | AssignStmt   (IdentExp (name, _), rhs, _) -> begin
      let VarEntry (ty, Some acc) = Symbol.lookup name env in
      let rhs = tr rhs in
      let ass = (trans_assign acc dst) in
      (rhs @ ass, env)
    end
  | AssignStmt   (lhs, rhs, _) -> begin
      let rinsts = translate_exp env rhs (dst::rest) in
      let (AddrIndirect (b, offset)) as addr,
          linsts = addr_of_exp lhs rest in
      let other::_ = rest in
      let check_null_insts = [add other b (OperImm(offset))] @ trans_call "wacc_check_pair_null" [other] in
      rinsts @ linsts @ check_null_insts @
      [F.str dst addr], env (* TODO need to handle storeb *)
    end
  | IfStmt       (cond, then_exp, else_exp, _) -> begin
      let env' = Symbol.new_scope env in
      let condi = tr cond in
      let theni, _ = translate env' frame regs then_exp in
      let elsei, _ = translate env' frame regs else_exp in
      let endi = trans_ifelse dst theni elsei in
      condi @ endi, env
    end
  | WhileStmt    (cond, body_stmt, _) -> begin
      let env' = Symbol.new_scope env in
      let condi = tr cond in
      let bodyi, _ = translate env' frame regs body_stmt in
      let while_cond_l = new_label ~prefix:"while_cond" () in
      let while_body_l = new_label ~prefix:"while_body" () in
      let while_end_l =  new_label ~prefix:"while_done" () in
      [Arm.LABEL(while_cond_l), None] @
      condi @
      [Arm.CMP(dst, Arm.OperImm 0), None;
       Arm.B(while_end_l), Some Arm.EQ;
       Arm.LABEL(while_body_l), None] @ bodyi @
      [Arm.B(while_cond_l), None;
       Arm.LABEL(while_end_l), None], env
    end
  | ExitStmt     (exp, _) -> begin
      let expi = tr exp in
      let ci = trans_call "wacc_exit" [dst] in
      expi @ ci , env
    end
  | VarDeclStmt  (ArrayTy ty, name, LiteralExp(LitArray elements, _), _) -> begin
      let open Arm in
      let array_length = List.length elements in
      let next::_ = rest in
      let addr_reg = dst in
      let element_size = 4 in
      let local_var = allocate_local frame 4 in
      let env' = Symbol.insert name (VarEntry (ArrayTy ty, Some local_var)) env in
      let size_offset = 4 in
      let insts = [MOV (dst, OperImm(element_size * array_length + 4)), None] @
                   trans_call "malloc" [dst] @
                  [MOV (dst, OperReg (reg_RV, None)), None ] @
                  (trans_assign local_var dst) (* holds address to the heap allocated array *)
      @ List.concat (List.mapi (fun i e -> (translate_exp env e rest)
                                           @ [STR (next, AddrIndirect (addr_reg, size_offset + element_size * i)), None]) elements) in
      let insts = insts @ [mov next (OperImm array_length);
                           str next (AddrIndirect (addr_reg, 0))] in
      insts, env'
    end
  | VarDeclStmt (ty, name, A.NewPairExp (exp, exp', _), _) -> begin
      (* TODO handle newpair in translate_exp? *)
      let (pair_addr::fst_v::snd_v::rest) = regs in
      let tr = translate_exp env in
      let local_var = allocate_local frame 4 in
      let env' = Symbol.insert name (VarEntry (ty, Some local_var)) env in
      let exp_insts = tr exp (fst_v::snd_v::rest) in
      let exp_ty = Semantic.check_exp env exp in
      let exp'_insts = tr exp' (snd_v::rest) in
      let exp'_ty = Semantic.check_exp env exp in
      let r0::r1::_ = rest in
      exp_insts @ exp'_insts @
      (* allocate for pair, each pair is represented with 4 * 2 bytes of addresses on the heap *)
      [mov pair_addr (OperImm(4 * 2))] @
      trans_call "malloc" [pair_addr] @
      [mov pair_addr (OperReg(reg_RV, None))] @
      (* fst allocation *)
      [mov r0 (OperImm(size_of_type exp_ty))] @
      trans_call "malloc" [r0] @
      [str reg_RV (AddrIndirect(pair_addr, 0));
       str fst_v (AddrIndirect(reg_RV, 0))] (* fixme handle byte store *) @
      (* snd allocation *)
      [mov r1 (OperImm(size_of_type exp'_ty))] @
      trans_call "malloc" [r1] @
      [str reg_RV (AddrIndirect(pair_addr, 4));
       str snd_v  (AddrIndirect(reg_RV, 0))]
      @ trans_assign local_var pair_addr, env'
    end
  | VarDeclStmt  (ty, name, exp, _) -> begin
      let size = size_of_type ty in
      let local_var = allocate_local frame size in
      let env' = Symbol.insert name (VarEntry (ty, Some local_var)) env in
      let expi = tr exp in
      let assigni = trans_assign (local_var) dst in
      expi @ assigni, env'
    end
  | SkipStmt      _ -> trans_noop, env
  | PrintStmt    (newline, exp, _) -> begin
      let expi = tr exp in
      let expt = S.check_exp env exp in
      let ty_str = match expt with
        | StringTy -> "string"
        | BoolTy -> "bool"
        | CharTy -> "char"
        | ArrayTy _ -> "array"
        | PairTy _ | PairTyy -> "pair"
        | IntTy -> "int"
        | _ -> assert false in
      let ci = trans_call ("wacc_print_" ^ ty_str) [dst] in
      let ci' = (if (newline) then
                   (trans_call("wacc_println") [])
                 else []) in
      expi @ ci @ ci', env
    end
  | RetStmt      (exp, _) -> begin
    let inst_list = tr exp in
    inst_list @ [mov (reg_RV) (OperReg (dst, None))] , env
    end
  | ReadStmt (exp, _) -> begin
      let addr, insts = addr_of_exp exp rest in
      let ty = Semantic.check_exp env exp in
      let ty_str = match ty with
        | IntTy -> "int"
        | CharTy -> "char"
        | _ -> invalid_arg "not valid read type"
      in
      let ci = trans_call ("wacc_read_" ^ ty_str) [] in
      let ci = ci @
               (if ty = CharTy then
                [Arm.STRB (Arm.reg_RV, addr), None]
                else [Arm.STR (Arm.reg_RV, addr), None]) in
      insts @ ci, env
    end
  | FreeStmt     (exp, _) -> begin
      let expi = tr exp in
      let ci = trans_call "wacc_free" [dst] in
      expi @ ci, env
    end
  | BlockStmt    (stmt, _) -> begin
      let env' = Symbol.new_scope env in
      let insts, _ = translate env' frame regs stmt in
      (insts, env)
    end

let print_insts (out: out_channel) (frame: frame) (insts: stmt list) =
  let open Printf in
  fprintf out ".data\n";
  List.iter (fun (l, s) ->
      fprintf out "%s" (sprintf "%s:\n\t.ascii \"%s\0\"\n" l (String.escaped s))) !strings;
  fprintf out ".text\n";
  fprintf out ".global main\n";
  fprintf out "main:\n";
  fprintf out "\tpush {lr}\n";
  let local_size: int = (Array.fold_left (+) 0 (Array.map (fun x -> match x with
      | InFrame (t, sz) -> sz) frame.frame_locals)) in
  fprintf out "%s" ("\tsub sp, sp, #" ^ (string_of_int local_size) ^ "\n");
  List.iter (fun x -> fprintf out "%s\n" (Arm.string_of_inst' x)) insts;
  fprintf out "%s" ("\tadd sp, sp, #" ^ (string_of_int local_size) ^ "\n") ;
  fprintf out "\tldr r0, =0\n";
  fprintf out "%s" "\tpop {pc}\n"

let rec translate_function_decs decs env inst_list =
  let open Arm in
  let env' = ref env in
  let allocate_in_regs reg = InReg(reg) in
  let rec allocate_field_list regs field_list env =
    match field_list with
    | [] -> ([], env)
    | [(ty, sym)] ->  begin
        let (first_reg::others) = regs in
        let local_var = allocate_in_regs first_reg in
        let env' = Symbol.insert sym (VarEntry(ty, Some local_var)) env in
        ([ty], env')
      end
    (*  | (ty, _)::r -> begin
        allocate_local frame (size_of_type ty);
        ty::allocate_field_list frame r
        end*)  in
  List.iter (fun f -> (let A.FuncDec (ty, ident, fields, stmt, pos) = f in
                       let tys = List.map fst fields in
                       env' := Symbol.insert ident (FuncEntry (ty, tys)) !env'
                      )) decs;
  let tr_func (Ast.FuncDec(retty, name, field_list, stmt, _)) = begin
    let frame = new_frame ("func_" ^ name) in
    let env'' = Symbol.new_scope !env' in
    let (arg_types, new_env) = allocate_field_list caller_saved_regs field_list env'' in
    let label_inst = labels ("f_" ^ name ) in
    let push_inst = push [reg_LR] in
    let pop_inst = pop [reg_PC] in
    let insts, _ = translate new_env frame callee_saved_regs stmt in (* FIXME *)
    let func_insts = label_inst::push_inst::(inst_list @ insts) in
    (func_insts @ [pop_inst]);
  end in
  !env', List.concat (List.map tr_func decs)

let translate_prog (decs, stmt) env out =
let open Printf in
  let frame = new_frame "main" in
  let (env', all_func_insts) = translate_function_decs decs env [] in
  let insts, _ = translate env' frame Arm.callee_saved_regs stmt in
  List.iter (fun x -> fprintf out "%s\n" (Arm.string_of_inst' x)) all_func_insts;
  ignore(print_insts out frame insts);
