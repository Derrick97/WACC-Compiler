module A = Ast_v2;;
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

let frame_size (frame:frame): int = (Array.fold_left (+) 0 (Array.map (fun x -> match x with
    | InFrame (t, sz) -> sz) frame.frame_locals))

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

let split_offset offset =
  if offset = 0 then (0,0) else
  let tempNum = ref 1 in
  let () = if offset < 256 then tempNum := offset * 2 in
  let () =
     while !tempNum <= offset do
     tempNum := !tempNum * 2
     done in
  (!tempNum / 2, offset - (!tempNum / 2))



let allocate_local (frame: frame) (size: size) =
  let offset = frame.frame_offset in
  let a = InFrame (offset, size) in
  frame.frame_offset <- offset + size;
  frame.frame_locals <- Array.append frame.frame_locals [|a|];
  a


let size_of_type = function
  | A.CharTy | A.BoolTy -> 1
  | A.IntTy -> 4
  | A.StringTy | A.PairTy _ | A.NullTy | A.PairTyy | A.ArrayTy _ -> 4 (* addresses *)

let trans_call
    ?(result: temp option)
    (fname: string)
    (args: temp list): (stmt list) = begin
  (* FIXME transfer the result back to some register *)
  let fname_label = new_namedlabel fname in
  let ilist = ref [] in
  let emit x = ilist := !ilist @ [x] in
  let open Arm in
  let rec zip a b = match (a, b) with
    | ([], _) -> []
    | (_, []) -> []
    | (x::xs, y::ys) -> (x, y)::(zip xs ys) in
  let split_reg_stack_passed regs =
    let reg_passed = ref [] in
    let stack_passed = ref [] in
    (List.iteri (fun i x -> if i <= 3
                 then reg_passed := !reg_passed @ [x]
                 else stack_passed := !stack_passed @ [x]) regs);
    !reg_passed, !stack_passed in
  let reg_passed, stack_passed = split_reg_stack_passed args in
  (* First we mov the first 4 registers *)
  emit(F.push caller_saved_regs);
  List.iter (fun (a, b) ->
      emit(F.mov a (Arm.OperReg (b, None)))) (zip F.caller_saved_regs reg_passed);
  (* The other registers are pushed to stack *)
  List.iter (fun x -> emit (Arm.push [x])) (List.rev stack_passed);
  !ilist @
  [F.bl fname_label] @
  (match result with
   | None -> []
   | Some t -> [mov t (OperReg(reg_RV, None))]) @
  [(F.pop caller_saved_regs)] @
  (List.map (fun x -> (pop [x])) (stack_passed));
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
  | InReg (reg_num) -> [F.mov reg_num (OperReg(rv,None))]
end

let trans_noop: stmt list = []

let rec translate_exp
    (env: E.env)
    (exp: A.exp)
    (regs: temp list)
  : (Arm.inst' list) =
  let tr = translate_exp env in
  let open Arm in
  let simple_exp = Simplify.simplify exp in
  let (exp', pos) = simple_exp in
  let check_overflow_inst = bl ~cond:VS "wacc_throw_overflow_error" in
  ((match regs with
   | (dst::rest) -> begin
       match exp' with
       | A.IdentExp (name) -> begin
           let E.VarEntry (t, Some acc) = Symbol.lookup name env in
	   let is_pair = function
	    | A.PairTy _ | A.PairTyy -> true
	    | _ -> false in
	   if is_pair t then
	     trans_var acc dst
	   else
             trans_var acc dst
         end
       | A.LiteralExp  (literal) -> begin match literal with
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
       | A.BinOpExp    (exp, binop, exp') -> begin
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
            | A.DivideOp -> trans_call ~result:dst "wacc_div" [dst;next]
            | A.AndOp -> [annd dst dst oper]
            | A.OrOp  -> [orr dst dst oper]
            | A.ModOp -> begin
                trans_call "wacc_mod" ~result:dst  [dst;next]
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
       | A.UnOpExp     (unop, exp) -> begin
           let ri = (translate_exp env exp regs) in
           let (fst_rest::others) = rest in
           ri @ (match unop with
               | A.NotOp -> [eor dst dst (OperImm 1)]
               | A.NegOp -> begin
                   [mov fst_rest (OperImm 0);
                    sub dst fst_rest (OperReg (dst, None));
                    check_overflow_inst]
                 end
               | A.LenOp -> trans_call ~result:dst "wacc_len" [dst]
               | A.OrdOp -> trans_call "wacc_ord" [dst]
               | A.ChrOp -> trans_call "wacc_chr" [dst])
         end
       | A.ArrayIndexExp (name, [exp]) -> begin
       let open Arm in
       let E.VarEntry (t, Some acc) = Symbol.lookup name env in
       let check_if_string some_ty = match some_ty with
         | Ast_v2.StringTy -> true
         | _-> false
       in
       let check_if_char_array some_ty = match some_ty with
         | Ast_v2.ArrayTy(CharTy) -> true
         | _-> false
       in
       let size = if check_if_string t then 1 else 4 in
       let index::addr::o::rest = rest in
       let skip_length_inst = if check_if_string t then [] else [add index index (OperImm 4)] in
       let insts = translate_exp env exp (index::rest)
          @ trans_var acc addr @ trans_call "wacc_check_array_bounds" [addr; index]
          @ [mov o (OperImm size);
             mul index index o]
          @ skip_length_inst
          @ [add addr addr (OperReg (index, None));
             load dst (AddrIndirect (addr, 0)) ]
              in insts
        (*   let open Arm in
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
              load dst (AddrIndirect (addr, 0)) ]*)
         end
       | A.ArrayIndexExp (name, explist) -> begin
           let open Arm in
           match explist with
           | fst_exp::snd_exp::others -> begin
             let fst_insts = translate_exp env (A.ArrayIndexExp(name, [fst_exp]),pos) regs in
             let index::addr::o::rests = rest in
             let inst = [mov addr (OperReg(dst,None))] @ translate_exp env snd_exp (index::rests) @ trans_call "wacc_check_array_bounds" [addr; index]
             @ [mov o (OperImm 4);
                mul index index o]
             @ [add addr addr (OperReg (index, None));
                load dst (AddrIndirect (addr, 0)) ]
             in fst_insts @ inst
           end
           | [] -> []
         end
      (* failwith "Only single dimensional array access is supported"*)
       | A.FstExp (exp) -> begin
           let (dst::next::rest) = regs in
           let insts = tr exp (regs) in
           insts @ trans_call "wacc_check_pair_null" [dst] @
           [load next (AddrIndirect(dst, 0));
            load dst (AddrIndirect(next, 0))]
           end
       | A.SndExp (exp) -> begin
           let (dst::next::rest) = regs in
           let insts = tr exp (regs) in (* dst stores address to pair *)
           insts @ trans_call "wacc_check_pair_null" [dst] @
           [load next (AddrIndirect(dst, 4)); (* this get address of the second element *)
            load dst (AddrIndirect(next, 0))] (* now we load it *)
         end
       | A.CallExp (fname, exp_list) -> begin
           let args, _, inst = List.fold_left
               (fun (used, r::rs, ins) e -> (used @ [r], rs, ins @ (tr e (r::rs))))
               ([], regs, []) exp_list in
           inst @ trans_call ~result:dst ("f_" ^ fname) args
        end
       | A.NullExp -> begin
           [mov dst (OperImm 0)]
         end
       | A.NewPairExp _ -> invalid_arg "newpair handled rhs"
     end
   | [] -> invalid_arg "Registers have run out"))

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
            translate_exp env h regs @  process_function_arguments sym r rest (dst::used_reg) env
            end

and translate (env: E.env)
    (frame: frame)
    (regs: temp list) (stmt: A.stmt): (Arm.inst' list * E.env) =
  let open Ast_v2 in
  let open Arm in
  let tr x = (translate_exp env x regs) in
  let dst::rest = regs in
  let addr_of_exp (e: A.exp) (regss) =
  let (exp', pos) = e in
  match exp' with
    | ArrayIndexExp (name, [exp]) -> begin
        let open Arm in
        let E.VarEntry (t, Some acc) = Symbol.lookup name env in
        let check_if_string some_ty = match some_ty with
          | StringTy -> true
          | _-> false
        in
        let check_if_char_array some_ty = match some_ty with
          | ArrayTy(CharTy) -> true
          | _-> false
        in
        let size = if check_if_string t || check_if_char_array t then 1 else size_of_type t in
        let index::addr::o::rest = regss in
        let skip_length_inst = if check_if_string t then [] else [add index index (OperImm 4)] in
        let insts = translate_exp env exp (index::rest)
           @ trans_var acc addr @ trans_call "wacc_check_array_bounds" [addr; index]
           @ [mov o (OperImm size);
              mul index index o]
           @ skip_length_inst @
              [add addr addr (OperReg (index, None))] in
        AddrIndirect (addr, 0), insts
      end
    | ArrayIndexExp _ -> assert false
    | IdentExp (name) -> begin
        let VarEntry (ty, Some (InFrame (offset, sz) as acc)) = (Symbol.lookup name env) in
	let is_pair = function
	  | A.PairTy _ | A.PairTyy -> true
	  | _ -> false in
	let insts = trans_var acc dst in
        AddrIndirect (Arm.reg_SP, offset), insts
      end
    | FstExp (exp) -> begin
        let dst::next::rest = regss in
        let insts = translate_exp env exp regss @ trans_call "wacc_check_pair_null" [dst] in
        AddrIndirect (next, 0), insts @ [load next (AddrIndirect (dst, 0))]
      end
    | SndExp (exp) -> begin
        let dst::next::rest = regss in
        let insts = translate_exp env exp regss @ trans_call "wacc_check_pair_null" [dst] in
        AddrIndirect (next, 0), insts @ [load next (AddrIndirect (dst, 4))]
      end
    | _ -> invalid_arg "Not an lvalue" in
  let (stmt', pos) = stmt in
  match stmt' with
  | SeqStmt (stmt, stmtlist) -> begin
      let exp,  env' = translate env frame regs stmt in
      let exp', env'' = (translate env' frame regs stmtlist) in
      (exp @ exp', env'')
    end
  | SideEffectStmt(exp, op) -> begin
      match op with
      | IncOp ->
        translate env frame regs (AssignStmt(exp, (BinOpExp(exp, PlusOp, (LiteralExp(LitInt(1)), pos)), pos)), pos)
      | DecOp ->
        translate env frame regs (AssignStmt(exp, (BinOpExp(exp, MinusOp, (LiteralExp(LitInt(1)), pos)), pos)), pos)
      end
  | TwoArgsSideEffectStmt(lhs, op, rhs) -> begin
      match op with
      | PlusEqOp ->
        translate env frame regs (AssignStmt(lhs, (BinOpExp(lhs, PlusOp, rhs), pos)), pos)
      | MinusEqOp ->
        translate env frame regs (AssignStmt(lhs, (BinOpExp(lhs, MinusOp, rhs), pos)), pos)
      | TimesEqOp ->
        translate env frame regs (AssignStmt(lhs, (BinOpExp(lhs, TimesOp, rhs), pos)), pos)
      end
  | AssignStmt   ((IdentExp (name), _), rhs) -> begin
      let VarEntry (ty, Some acc) = Symbol.lookup name env in
      let rhs = tr rhs in
      let ass = (trans_assign acc dst) in
      (rhs @ ass, env)
    end
  | AssignStmt   (lhs, rhs) -> begin
      let rinsts = translate_exp env rhs (dst::rest) in
      let (AddrIndirect (b, offset)) as addr,
          linsts = addr_of_exp lhs rest in
      let other::_ = rest in
      let check_null_insts = [add other b (OperImm(offset))] @ trans_call "wacc_check_pair_null" [other] in
      let insts = rinsts @ linsts @ check_null_insts in
      let str_inst = if size_of_type (Semantic.check_exp env rhs) = 4 then
      [F.str dst addr] else [F.strb dst addr] in (* TODO need to handle storeb *)
      (insts @ str_inst, env)
    end
  | IfStmt       (cond, then_exp, else_exp) -> begin
      let env' = Symbol.new_scope env in
      let condi = tr cond in
      let theni, _ = translate env' frame regs then_exp in
      let elsei, _ = translate env' frame regs else_exp in
      let endi = trans_ifelse dst theni elsei in
      condi @ endi, env
    end
  | WhileStmt    (cond, body_stmt) -> begin
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
  | ExitStmt     (exp) -> begin
      let expi = tr exp in
      let ci = trans_call "wacc_exit" [dst] in
      expi @ ci , env
    end
  | VarDeclStmt  (ArrayTy ty, name, (LiteralExp(LitArray elements),_)) -> begin
      let open Arm in
      let array_length = List.length elements in
      let next::_ = rest in
      let addr_reg = dst in
      let element_size = 4 in
      let local_var = allocate_local frame 4 in
      let env' = Symbol.insert name (VarEntry (ArrayTy ty, Some local_var)) env in
      let size_offset = 4 in
      let insts = [MOV (dst, OperImm(element_size * array_length + 4)), None] @
                   trans_call ~result:dst "malloc" [dst] @
                  (trans_assign local_var dst) (* holds address to the heap allocated array *)
      @ List.concat (List.mapi (fun i e -> (translate_exp env e rest)
                                           @ [STR (next, AddrIndirect (addr_reg, size_offset + element_size * i)), None]) elements) in
      let insts = insts @ [mov next (OperImm array_length);
                           str next (AddrIndirect (addr_reg, 0))] in
      insts, env'
    end
  | VarDeclStmt (ty, name, (A.NewPairExp (exp, exp'),_)) -> begin
      (* TODO handle newpair in translate_exp? *)
      let (pair_addr::fst_v::snd_v::tmp::rest) = regs in
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
      trans_call ~result:pair_addr "malloc" [pair_addr] @
      (* fst allocation *)
      [mov r0 (OperImm(size_of_type exp_ty))] @
      trans_call ~result:tmp "malloc" [r0] @
      [str tmp (AddrIndirect(pair_addr, 0));
       str fst_v (AddrIndirect(tmp, 0))] (* fixme handle byte store *) @
      (* snd allocation *)
      [mov r1 (OperImm(size_of_type exp'_ty))] @
      trans_call ~result:tmp "malloc" [r1] @
      [str tmp (AddrIndirect(pair_addr, 4));
       str snd_v  (AddrIndirect(tmp, 0))]
      @ trans_assign local_var pair_addr, env'
    end
  | VarDeclStmt  (ty, name, exp) -> begin
      let size = size_of_type ty in
      let local_var = allocate_local frame size in
      let env' = Symbol.insert name (VarEntry (ty, Some local_var)) env in
      let expi = tr exp in
      let assigni = trans_assign (local_var) dst in
      expi @ assigni, env'
    end
  | SkipStmt      _ -> trans_noop, env
  | PrintStmt    (newline, exp) -> begin
      let expi = tr exp in
      let expt = S.check_exp env exp in
      let ty_str = match expt with
        | StringTy -> "string"
        | BoolTy -> "bool"
        | CharTy -> "char"
        | ArrayTy CharTy -> "char_array"
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
  | RetStmt      (exp) -> begin
      let inst_list = tr exp in
      inst_list @ [mov (reg_RV) (OperReg (dst, None));
                   pop (callee_saved_regs);
                   add reg_SP reg_SP (OperImm (frame_size frame));
                   pop [reg_PC]; (LTORG, None)
                  ], env
    end
  | ReadStmt (exp) -> begin
      let addr, insts = addr_of_exp exp rest in
      let ty = Semantic.check_exp env exp in
      let ty_str = match ty with
        | IntTy -> "int"
        | CharTy -> "char"
        | _ -> invalid_arg "not valid read type"
      in
      let ci = trans_call ~result:dst ("wacc_read_" ^ ty_str) [] in
      let ci = ci @
               (if ty = CharTy then
                [Arm.STRB (dst, addr), None]
                else [Arm.STR (dst, addr), None]) in
      insts @ ci, env
    end
  | FreeStmt     (exp) -> begin
      let expi = tr exp in
      let ci = trans_call "wacc_free" [dst] in
      expi @ ci, env
    end
  | BlockStmt    (stmt) -> begin
      let env' = Symbol.new_scope env in
      let insts, _ = translate env' frame regs stmt in
      (insts, env)
    end

let recompute_allocation frame insts =
  let open Arm in
  let localsize = frame_size frame in
  List.map (fun i -> match i with
      | (ADD (o1, o2, OperImm (n)), cond) when o1 = reg_SP && o2 = reg_SP ->
        (ADD (o1, o2, OperImm (localsize)), cond)
      | _ -> i
    ) insts

let print_insts (out: out_channel) (frame: frame) (insts: stmt list) =
  let open Printf in
  fprintf out ".data\n";
  List.iter (fun (l, s) ->
      fprintf out "%s" (sprintf "%s:\n\t.ascii \"%s\0\"\n" l (String.escaped s))) !strings;
  fprintf out ".text\n";
  fprintf out ".global main\n";
  fprintf out "main:\n";
  fprintf out "\tpush {lr}\n";
  let local_size = frame_size frame in
  let (valid_size1, valid_size2) = split_offset local_size in
  fprintf out "%s" ("\tsub sp, sp, #" ^ (string_of_int valid_size1) ^ "\n");
  if valid_size2 !=0 then
  fprintf out "%s" ("\tsub sp, sp, #" ^ (string_of_int valid_size2) ^ "\n");
  List.iter (fun x -> fprintf out "%s\n" (Arm.string_of_inst' x)) insts;
  fprintf out "%s" ("\tadd sp, sp, #" ^ (string_of_int valid_size1) ^ "\n") ;
  if valid_size2 !=0 then
  fprintf out "%s" ("\tadd sp, sp, #" ^ (string_of_int valid_size2) ^ "\n") ;
  fprintf out "\tldr r0, =0\n";
  fprintf out "%s" "\tpop {pc}\n";
  fprintf out "%s" "\t.ltorg\n"


let rec translate_function_decs decs env inst_list =
  let open Arm in
  let env' = ref env in
  List.iter (fun f -> (let (A.FuncDec (ty, ident, fields, stmt), pos) = f in
                       let tys = List.map fst fields in
                       env' := Symbol.insert ident (FuncEntry (ty, tys)) !env'
                      )) decs;
  let tr_func ((Ast_v2.FuncDec(retty, name, field_list, stmt),pos)) = begin
    let frame = new_frame ("func_" ^ name) in
    let env'' = ref (Symbol.new_scope !env') in
    let offset = ref 0 in
    ignore(List.mapi (fun i (ty, name) -> begin
          if (i <= 3) then
            let acc = InReg (List.nth caller_saved_regs i) in
            env'' := Symbol.insert name (VarEntry (ty, Some acc)) !env'';
            acc
          else
            let sz = size_of_type ty in
            let acc = InFrame (!offset, sz) in
            env'' := Symbol.insert name (VarEntry (ty, Some acc)) !env'';
            offset := !offset - sz;
            acc
        end) field_list);
    let label_inst = labels ("f_" ^ name ) in
    let push_inst = push [reg_LR] in
    let pop_inst = pop [reg_PC] in

    let push_reg = push callee_saved_regs in
    let pop_reg = pop callee_saved_regs in

    let insts, _ = translate !env'' frame callee_saved_regs stmt in (* FIXME *)
    let local_size: int = (Array.fold_left (+) 0 (Array.map (fun x -> match x with
        | InFrame (t, sz) -> sz) frame.frame_locals)) in
    let alloc_insts = [(sub reg_SP reg_SP (OperImm local_size))] in
    let dealloc_insts = [(add reg_SP reg_SP (OperImm local_size))] in
    let func_insts = label_inst::push_inst::push_reg::(alloc_insts@(inst_list @ insts)) in
    (recompute_allocation frame (func_insts @ dealloc_insts @ [pop_reg] @ [pop_inst] @ [(LTORG, None)]));
  end in
  !env', List.concat (List.map tr_func decs)

let translate_prog (decs, stmt) env out =
  let open Printf in
  let frame = new_frame "main" in
  let (env', all_func_insts) = translate_function_decs decs env [] in
  let insts, _ = translate env' frame Arm.callee_saved_regs stmt in
  List.iter (fun x -> fprintf out "%s\n" (Arm.string_of_inst' x)) all_func_insts;
  ignore(print_insts out frame insts);
