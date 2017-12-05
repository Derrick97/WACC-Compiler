module IL = Il;;
module A = Ast_v2;;
module E = Env;;
module S = Semantic;;
module F = Arm;;
module RA = Register_allocator;;
open Env;;

type size = int
type il = IL.il
type temp = Temp.temp
type access = Env.access
type ctx = Env.env

type frame = {
  mutable frame_counter: int;
  mutable frame_offset: int;
  mutable frame_locals: access array;
}

type frag =
  | STRING of Temp.label * string
  | PROG of string * il list

let frame_size (frame:frame): int
  = Array.(frame.frame_locals
    |> (map (fun x -> match x with
        | InFrame (t, sz) -> sz
        | _ -> invalid_arg "not in frame"))
    |> (fold_left (+) 0))

let (frags: frag list ref) = ref []
let counter = ref 0
let strings = ref []

let split_offset offset =
  if offset = 0 then (0,0)
  else
    let temp_num = ref 1 in
    let () = if offset < 256 then temp_num := offset * 2 in
    let () =
      while !temp_num <= offset do
        temp_num := !temp_num * 2
      done in
    (!temp_num / 2, offset - (!temp_num / 2))

let iter_locals (env) f =
  let f = function
    | VarEntry (A.PairTy _, Some acc) -> f acc
    | _ -> () in
  Symbol.iter_local env f

let is_pair = function
  | A.PairTy _ | A.PairTyy -> true
  | _ -> false

let is_string ty = match ty with
  | A.StringTy -> true
  | _ -> false

let is_char_array ty = match ty with
  | A.ArrayTy(A.CharTy) -> true
  | _ -> false

let is_noop = function
  | IL.NOOP -> true
  | _ -> false

let string_of_ty =
  let open A in
  function
  | StringTy -> "string"
  | BoolTy -> "bool"
  | CharTy -> "char"
  | ArrayTy CharTy -> "char_array"
  | ArrayTy _ -> "array"
  | PairTy _ | PairTyy -> "pair"
  | IntTy -> "int"
  | _ -> assert false

let builtin_func =
  ["wacc_len"; "wacc_ord"; "wacc_mod"; "wacc_chr"; "wacc_div"; "wacc_exit"]

let mk_fname fname = "f_"^fname

let rec zip a b = match (a, b) with
  | ([], _) -> []
  | (_, []) -> []
  | (x::xs, y::ys) -> (x, y)::(zip xs ys)

let new_label ?(prefix="L") (): string =
  let i = !counter in
  counter := !counter + 1;
  prefix ^ (string_of_int i)

let new_namedlabel name = name

let new_frame frame = {
  frame_counter = 0;
  frame_offset = 0;
  frame_locals = [| |];
}

let oper_reg r = Il.OperReg r
let oper_imm i = Il.OperImm i

let addr_indirect r offset = (Il.ADDR_INDIRECT (r, offset))
let addr_label l = (Il.ADDR_LABEL l)

let allocate_local (frame: frame) (size: size) =
  let offset = frame.frame_offset in
  let a = InFrame (offset, size) in
  frame.frame_offset <- offset + size;
  frame.frame_locals <- Array.append frame.frame_locals [|a|];
  a

let allocate_temp () = begin
  let c = !counter in
  counter := c+1;
  "t" ^ (string_of_int c)
end

let size_of_type = function
  | A.CharTy | A.BoolTy -> 1
  | A.IntTy -> 4
  | A.StringTy | A.PairTy _ | A.NullTy | A.PairTyy | A.ArrayTy _ -> 4

let global_env: (Env.env option ref) = ref None

let begin_scope () = match !global_env with
  | Some scope ->
      global_env := Some (Symbol.new_scope scope)
  | _ -> invalid_arg "global env not initialized"

let end_scope () = match !global_env with
  | Some scope ->
      global_env := Symbol.parent scope
  | _ -> invalid_arg "global env not initialized";;

let emitters = Stack.create ();;
let emitter():Il.emitter = Stack.top emitters;;
let push_new_emitter () = Stack.push (IL.new_emitter()) emitters;;
let pop_emitter () = Stack.pop emitters;;
let emit i = Il.append (emitter()) i;;

push_new_emitter();;



let add o1 o2 =
  let t = allocate_temp() in
  emit(IL.add t o1 o2); t
and sub o1 o2 =
  let t = allocate_temp() in
  emit(IL.sub t o1 o2); t
and loadq addr =
  let t = allocate_temp() in
  emit(IL.load IL.WORD t addr); t
and loadb addr =
  let t = allocate_temp() in
  emit(IL.load IL.BYTE t addr); t
and storeq t addr =
  emit(IL.store IL.WORD t addr)
and storeb t addr =
  let t = allocate_temp() in
  emit(IL.store IL.BYTE t addr)
and and_ o1 o2 =
  let t = allocate_temp() in
  emit(IL.and_ t o1 o2)
and or_ o1 o2 =
  let t = allocate_temp() in
  emit(IL.or_ t o1 o2)
and mov o1 o2 =
  emit(IL.mov o1 o2)



let rec trans_var (var: access): temp =
  let open Il in
  let t = allocate_temp() in
  match var with
  | InFrame (offset, sz) ->
      (match sz with
       | 4 -> emit(load WORD t (ADDR_INDIRECT (Arm.reg_SP, offset))); t
       | 1 -> emit(load BYTE t (ADDR_INDIRECT (Arm.reg_SP, offset))); t
       | _ -> assert false)
  | InReg r -> emit(Il.MOV (t, Il.OperReg r)); t

and trans_assign
    (lv: access)
    (rv: temp) = begin
  let open IL in
  match lv with
  | InFrame (offset, sz) -> (
      match sz with
      | 4 -> emit(store WORD rv (ADDR_INDIRECT (Arm.reg_SP, offset)))
      | 1 -> emit(store BYTE rv (ADDR_INDIRECT (Arm.reg_SP, offset)))
      | _ -> assert false)
  | InReg (dst) -> emit(mov dst (OperReg rv))
end

and trans_mark (ctx) (frame: frame) = begin
  iter_locals ctx (fun acc -> begin
        match acc with
        | InFrame (offset, sz) -> begin
            let t = allocate_temp() in
            emit(Il.load Il.WORD t (addr_indirect F.reg_SP offset));
            ignore( trans_call ctx frame "mark_all" [t]);
          end
        | _ -> invalid_arg "not stack local variable"
      end);
end

and trans_sweep (ctx) (frame: frame): unit =
  ignore ((trans_call ctx frame "sweep_all" []));

and gc (ctx) (frame): unit =
  ignore(trans_mark ctx frame);
  ignore(trans_sweep ctx frame)

and get_access env name = match Symbol.lookup name env with
  | E.VarEntry (t, Some acc) -> (t, acc)
  | _ -> invalid_arg "not an access"

and allocate_heap env frame ty =
  let open IL in
  let t = allocate_temp() in
  match ty with
  | A.IntTy ->
    (emit(mov t (oper_imm (0)));
    trans_call env frame "allocate" [t])
  | A.PairTy _ | A.PairTyy -> (emit(mov t (oper_imm (1)));
                               trans_call env frame "allocate" [t])
  | _ -> (
      emit(mov t (oper_imm (size_of_type ty)));
      trans_call env frame "malloc" [t])
and trans_exp ctx frame exp =
  let open Il in
  let (exp, pos) = exp in
  let tr = trans_exp ctx frame in
  let dst = allocate_temp() in
  begin match exp with
    | A.IdentExp       name -> begin
        let (t, acc) = get_access ctx name in
        trans_var acc
      end
    | A.ArrayIndexExp (ident, exps) -> begin
        let (t, acc) = get_access ctx ident in
        let size = if is_string t then 1 else 4 in
        let index = allocate_temp() in
        emit(mov index (oper_imm 0)); (* initializing index register *)
        let o = allocate_temp() in
        let sz = if not (is_string t)
          then 4 else 0 in (* for char arrays we do not store length *)
        let addr = trans_var acc in
        let rec emit_addressing = (function
            | exp::other -> begin
                let index = tr exp in
                ignore(trans_call ctx frame "wacc_check_array_bounds" [addr; index]);
                List.iter (emit) ([mov o (oper_imm size);
                      mul index (oper_reg index) (oper_reg o); (* compute offset *)
                      add addr  (oper_reg addr)  (oper_reg index);
                      add addr (oper_reg addr) (oper_imm sz);
                      load WORD addr (addr_indirect addr 0)]);
                emit_addressing other
              end
            | [] -> ()) in
        emit_addressing exps;
        emit(mov dst (oper_reg addr));
        dst
        end
    | A.LiteralExp    (lit) -> begin
        let open Il in
        (match lit with
            | A.LitInt i -> emit(load WORD dst (ADDR_LABEL (string_of_int i)))
            | A.LitString s -> begin
                let label = new_label() in
                frags := (STRING (label, s))::!frags;
                emit(load WORD dst (ADDR_LABEL label))
              end
            | A.LitBool b -> if b then
                  emit(mov dst (OperImm 1))
                else
                  emit(mov dst (OperImm 0))
            | A.LitChar c -> emit(mov dst (OperImm (Char.code c)))
            | _ -> assert false);
        dst
      end
    | A.BinOpExp      ((lhs), binop, (rhs)) -> begin
        let (l) = (tr lhs) in
        let (r) = (tr rhs) in
        let (opl, opr) = (oper_reg l), (oper_reg r) in
        (match binop with
            | A.PlusOp   -> emit (add dst opl opr)
            | A.MinusOp  -> emit (sub dst opl opr)
            | A.TimesOp  -> emit (mul dst opl opr)
            | A.DivideOp -> assert false
            | A.AndOp ->    emit (and_ dst opl opr)
            | A.OrOp  ->    emit (or_ dst opl opr)
            | A.EqOp -> emit (sub dst opl opr); emit(cmp Il.EQ dst (oper_reg dst) (oper_imm 0))
            | A.NeOp -> emit (sub dst opl opr); emit(cmp Il.NE dst (oper_reg dst) (oper_imm 0))
            | A.GeOp -> emit (sub dst opl opr); emit(cmp Il.GE dst (oper_reg dst) (oper_imm 0))
            | A.GtOp -> emit (sub dst opl opr); emit(cmp Il.GT dst (oper_reg dst) (oper_imm 0))
            | A.LeOp -> emit (sub dst opl opr); emit(cmp Il.LE dst (oper_reg dst) (oper_imm 0))
            | A.LtOp -> emit (sub dst opl opr); emit(cmp Il.LT dst (oper_reg dst) (oper_imm 0))
            | A.ModOp -> invalid_arg "mod should be handled in frontend");
        dst
      end

    | A.UnOpExp       (A.NegOp, exp) -> begin
        (* FIXME a problem with the limitation of ARM architecture *)
        trans_exp ctx frame ((A.BinOpExp ((A.LiteralExp (A.LitInt 0), pos), A.MinusOp, exp), pos))
      end
    | A.UnOpExp       (unop, exp) -> begin
        let (t) = trans_exp ctx frame exp in
        (match unop with
            | A.NotOp -> emit(eor t (oper_reg t) (oper_imm 1))
            | A.NegOp -> emit(sub t (oper_imm 0) (oper_reg t))
            | A.LenOp | A.OrdOp | A.ChrOp | A.IncOp
              -> failwith "should be desugared" );
        t
      end
    | A.CallExp       (fname, args) -> begin
        let argtemps = List.map (fun arge -> tr arge) args in
        let resultt =
          if (List.length (List.filter (fun x -> x = fname) builtin_func) = 0)
          then
            trans_call ctx frame (mk_fname fname) argtemps
          else
            trans_call ctx frame (fname) argtemps
        in
        resultt
      end
    | A.NewPairExp    (lval, exp) -> failwith "newpair only on rhs"
    | A.FstExp         exp  -> begin
        let expt = tr exp in    (* this obtains a node *)
        let _ = (trans_call ctx frame "wacc_check_pair_null" [expt]) in
        let fstaddr = trans_call ctx frame "pair_fst_get" [expt] in
        let out = trans_call ctx frame "node_get_data" [fstaddr] in
        out
      end
    | A.SndExp         exp  -> begin
        let expt = tr exp in
        let _ =  (trans_call ctx frame "wacc_check_pair_null" [expt]) in
        let sndaddr = trans_call ctx frame "pair_snd_get" [expt] in
        let out = trans_call ctx frame "node_get_data" [sndaddr] in
        out
      end
    | A.NullExp -> emit(mov dst (oper_imm 0)); dst
  end

and trans_call (ctx: ctx)
    (fr: frame)
    (fname: string)
    (args: temp list) = begin
  let open Il in
  let fname_label = new_namedlabel fname in
  let result = allocate_temp() in
  let split_reg_stack_passed regs =
    let reg_passed = ref [] in
    let stack_passed = ref [] in
    List.iteri (fun i x -> if i <= List.length Arm.caller_saved_regs
                 then reg_passed := !reg_passed @ [x]
                 else stack_passed := !stack_passed @ [x]) regs;
    !reg_passed, !stack_passed
  in
  let reg_passed, stack_passed = split_reg_stack_passed args in
  (* First we mov the first 4 registers *)
  emit(push Arm.caller_saved_regs);
  List.iter (fun (d, s) -> emit(mov d (Il.OperReg s))) (zip F.caller_saved_regs reg_passed);
  (* The other registers are pushed to stack *)
  List.iter (fun x -> emit (push [x])) (List.rev stack_passed);
  emit(call fname_label);
  emit(mov result (oper_reg F.reg_RV));
  emit(pop F.caller_saved_regs);
  (List.iter (fun x -> emit(Il.POP [x])) (stack_passed));
  result
end

and addr_of_exp (env) frame (e: A.exp): (Il.addr) =
  let open A in
  let open Il in
  let (exp', pos) = e in
  match exp' with
  | ArrayIndexExp (name, [exp]) -> begin
      let t, acc = get_access env name in
      let size = if is_string t || is_char_array t then 1 else size_of_type t in
      let o = allocate_temp() in
      let index = trans_exp env frame exp in
      let addr = trans_var acc in
      let _ = trans_call env frame "wacc_check_array_bounds" [addr; index] in
      emit(mov o (oper_imm size));
      emit(mul index (oper_reg index) (oper_reg o));
      if is_string t then () else
          emit(add index (oper_reg index) (oper_imm 4));
      emit (add addr (oper_reg addr) (oper_reg index));
      Il.ADDR_INDIRECT (addr, 0)
    end
  | ArrayIndexExp _ -> assert false
  | IdentExp (name) -> begin
      let offset = match Symbol.lookup name env with
      | VarEntry (ty, Some (InFrame (offset, sz))) -> offset
      | _ -> failwith "not a variable" in
      (addr_indirect Arm.reg_SP offset)
    end
  | FstExp (exp) -> begin
      let dst = trans_exp env frame exp in
      let _ = trans_call env frame "wacc_check_pair_null" [dst] in
      let fstaddr = trans_call env frame "pair_fst_get" [dst] in
      (* emit (load WORD dst (addr_indirect fstaddr 0)); *)
      (addr_indirect fstaddr 0)
    end
  | SndExp (exp) -> begin
      let dst = trans_exp env frame exp  in
      let _ = trans_call env frame "wacc_check_pair_null" [dst] in
      emit(load WORD dst (addr_indirect dst 4));
      (addr_indirect dst 0)
    end
  | _ -> invalid_arg "Not an lvalue"

let exp_type env exp =
   match exp with
  | A.CallExp ("wacc_len", args),_ -> A.IntTy
  | A.CallExp ("wacc_div", args),_ -> A.IntTy
  | A.CallExp ("wacc_mod", args),_ -> A.IntTy
  | A.CallExp ("wacc_chr", args),_ -> A.CharTy
  | A.CallExp ("wacc_ord", args),_ -> A.IntTy
  | _ -> Semantic.check_exp env exp

let rec trans_stmt env frame stmt: ctx = begin
  let open Il in

  let tr = trans_exp env frame in
  let (stmt, _) = stmt in
  let get_access = get_access env in
  match stmt with
  | A.SeqStmt (stmt, stmtlist) -> begin
      let env' = trans_stmt env frame stmt in
      let env'' = (trans_stmt env' frame stmtlist) in
      (env'')
    end
  | A.PrintStmt (newline, exp) -> begin
      let v = trans_exp env frame exp in
      let expt = exp_type env exp in
      let _ = trans_call env frame ("wacc_print_" ^ (string_of_ty expt)) [v]; in
      if newline then
        (ignore (trans_call env frame "wacc_println" []))
      else ();
      env
    end
  | A.AssignStmt   ((A.IdentExp (name), _), rhs) -> begin
      let (ty, acc) = get_access name in
      let rhst = tr rhs in
      (trans_assign acc rhst);
      env
    end
  | A.AssignStmt   (lhs, rhs) -> begin
      let rhst = trans_exp env frame rhs in
      let laddr = addr_of_exp env frame lhs in
      (if size_of_type (Semantic.check_exp env rhs) = 4 then
         emit (store WORD rhst laddr) else
         emit (store BYTE rhst laddr));
      env
    end
  | A.VarDeclStmt  (A.ArrayTy ty, name, (A.LiteralExp (A.LitArray elements), _)) -> begin
      let local_var = allocate_local frame 4 in
      let env' = Symbol.insert name (VarEntry (A.ArrayTy ty, Some local_var)) env in
      let array_length = List.length elements in
      let element_size = 4 in
      let dst = allocate_temp() in
      let next = allocate_temp() in
      emit(mov dst (oper_imm (array_length * element_size + 4)));
      let addr_reg = trans_call env frame "malloc" [dst] in
      List.iteri (fun i item -> begin
            let t = tr (item) in
            emit(store WORD t (addr_indirect addr_reg (element_size * i + 4)));
          end) elements;
      emit(mov next (oper_imm array_length));
      emit(store WORD next (addr_indirect addr_reg 0));
      trans_assign local_var addr_reg;
      env'
    end
  | A.VarDeclStmt (ty, name, (A.NewPairExp (exp, exp'),_)) -> begin
      (* allocate for pair, each pair is represented with 4 * 2
         bytes of addresses on the heap *)
      let local_var = allocate_local frame 4 in
      let env' = Symbol.insert name (VarEntry (ty, Some local_var)) env in
      let sizet = allocate_temp() in
      let fstt = tr exp in
      let sndt = tr exp' in
      let exp_ty = exp_type env exp in
      let exp'_ty = exp_type env exp' in
      let r0, r1 = allocate_temp(), allocate_temp() in
      (* malloc for pair address space *)
      (* emit(mov sizet (oper_imm (4 * 2))); *)
      let pair_addrt = allocate_heap env frame A.PairTyy in
      (* emit(store WORD pair_addrt (addr_indirect (F.reg_SP) 0)); *)
      trans_assign local_var pair_addrt;
      (* fst allocation *)
      (* emit(mov r0 (oper_imm (size_of_type exp_ty))); *)
      let fst_addrt = allocate_heap env frame exp_ty in
      (* emit(store WORD fst_addrt (addr_indirect pair_addrt 0));
       * emit(store WORD fstt (addr_indirect fst_addrt 0)); *)
      ignore(trans_call env frame "node_set_data" [fst_addrt; fstt]);
      ignore(trans_call env frame "pair_fst_set" [pair_addrt; fst_addrt]);
      (* snd allocation *)
      (* emit(mov r1 (oper_imm (size_of_type exp'_ty))); *)
      let snd_addrt = allocate_heap env frame exp'_ty in
      (* emit(store WORD snd_addrt  (addr_indirect pair_addrt 4));
       * emit(store WORD sndt (addr_indirect snd_addrt 0)); *)
      ignore(trans_call env frame "node_set_data" [snd_addrt; sndt]);
      ignore(trans_call env frame "pair_snd_set" [pair_addrt; snd_addrt]);
      (* Trigger gc *)
      gc env' frame;
      env'
    end
  | A.VarDeclStmt  (ty, name, exp) -> begin
      let size = size_of_type ty in
      let local_var = allocate_local frame size in
      let env' = Symbol.insert name (VarEntry (ty, Some local_var)) env in
      let expt = tr exp in
      let () = trans_assign (local_var) expt in
      gc env' frame;
      env'
    end
  | A.SkipStmt       -> env
  | A.CallStmt (exp) -> ignore (tr exp); env
  | A.ReadStmt (exp) -> begin
      let addr = addr_of_exp env frame exp in
      let ty = exp_type env exp in
      let readt = trans_call env frame ("wacc_read_" ^ (string_of_ty ty)) [] in
      (if ty = A.CharTy then
         emit(store BYTE readt addr)
       else
         emit(store WORD readt addr));
      env
    end
  | A.FreeStmt     (exp) -> begin
      let expt = tr exp in
      let _ = trans_call env frame "wacc_free" [expt] in
      env
    end
  | A.IfStmt       (cond, then_exp, else_exp) -> begin
      let open Il in
      let true_l = new_label ~prefix:"if_then" () in
      let false_l = new_label ~prefix:"if_else" () in
      let end_l = new_label ~prefix:"if_end" () in
      let env' = Symbol.new_scope env in
      let cond = tr cond in
      emit(cbr cond true_l false_l);
      emit(label true_l);
      ignore (trans_stmt env' frame then_exp);
      emit(jump  end_l);
      emit(label false_l);
      ignore (trans_stmt env' frame else_exp);
      emit(label end_l);
      env
    end
  | A.WhileStmt    (cond, body_stmt) -> begin
      let env' = Symbol.new_scope env in
      let while_cond_l = new_label ~prefix:"while_cond" () in
      let while_body_l = new_label ~prefix:"while_body" () in
      let while_end_l =  new_label ~prefix:"while_done" () in
      emit(label (while_cond_l));
      let condt = tr cond in
      emit(cbr condt while_body_l while_end_l);
      emit(label (while_body_l));
      ignore (trans_stmt env' frame body_stmt);
      emit(jump (while_cond_l));
      emit(label while_end_l);
      env
    end
  | A.RetStmt (exp) -> begin
      let open F in
      let open Il in
      let expt = tr exp in
      emit(mov (reg_RV) (oper_reg expt));
      emit(add reg_SP (oper_reg reg_SP) (oper_imm (frame_size frame)));
      emit(pop [reg_PC]);
      env
    end
  | A.BlockStmt (body) -> begin
      let env' = Symbol.new_scope env in
      let _ = trans_stmt env' frame body in
      env
    end
  | A.ExitStmt (exp) -> failwith "exit should be desugared"
end

and pp_string out (l, s): unit =
  let open Printf in
  fprintf out "%s" (sprintf "%s:\n\t.ascii \"%s\0\"\n" l (String.escaped s))

and pp_inst out (i: Arm.inst') =
  Printf.fprintf out "%s\n" (Arm.string_of_inst' i)

and frame_prologue env (frame: frame): il list = begin
  let open IL in
  let local_size = frame_size frame in
  let (valid_size1, valid_size2) = split_offset local_size in
  let allocate_insts = if (local_size > 0) then
      [sub F.reg_SP (oper_reg F.reg_SP) (oper_imm valid_size1)]
    else [] in
  let handle_big_local_size_inst =
  (if (valid_size2 != 0) then
      [sub F.reg_SP (oper_reg F.reg_SP) (oper_imm valid_size2)] else [])
  in
  let insts = [push [F.reg_LR]] @ allocate_insts @ handle_big_local_size_inst in
  insts
end

and frame_epilogue env (frame: frame): il list = begin
  let open IL in
  let local_size = frame_size frame in
  let (valid_size1, valid_size2) = split_offset local_size in
  let deallocate_insts = if (local_size > 0) then
      [add F.reg_SP (oper_reg F.reg_SP) (oper_imm valid_size1)]
    else [] in
  let handle_big_local_size_inst =
  if (valid_size2 !=0) then
      [add F.reg_SP (oper_reg F.reg_SP) (oper_imm valid_size2)] else []
  in
  deallocate_insts @ handle_big_local_size_inst @
  [pop [F.reg_PC]]
end

and fixup_allocation frame insts =
  let open Il in
  let localsize = frame_size frame in
  Array.map (fun i -> match i with
      | (ADD (o1, OperReg o2, OperImm n)) when o1 = F.reg_SP && o2 = F.reg_SP ->
        (ADD (o1, OperReg o2, OperImm localsize))
      | _ -> i) insts

and add_function_decs decs env =
  let env = ref env in
  let add_function_def f = begin
    let (A.FuncDec (retty, ident, fields, stmt), _) = f in
    let argtys = List.map fst fields in
     env := Symbol.insert ident (FuncEntry (retty, argtys)) !env
  end in
  List.iter add_function_def decs;
  !env

and trans_function_declaration (env: ctx) (dec) = begin
  push_new_emitter();
  let ((A.FuncDec(retty, fname, arglist, body), _)) = dec in
  let body = Simplify.simplify_stmt body in
  let frame = new_frame ("func_" ^ fname) in
  let env' = ref (Symbol.new_scope env) in
  let add_binding name ty acc = env' := Symbol.insert name (VarEntry (ty, Some acc)) !env' in
  let offset = ref 0 in
  (* add arguments to new environment *)
  List.iteri (fun i (ty, name) -> begin
        if (i <= 3) then
          let acc = InReg (List.nth F.caller_saved_regs i) in
          add_binding name ty acc;
        else
          let sz = size_of_type ty in
          let acc = InFrame (!offset, sz) in
          offset := !offset - sz;
          add_binding name ty acc;
        end) arglist;
  (* translate function body *)
  let env'' = trans_stmt !env' frame body in
  (* fixup_allocation frame; *)
  let prologue = (frame_prologue env'' frame) in
  let epilogue =(frame_epilogue env'' frame) in
  (* view shift of local variables *)
  let code = ((emitter()).IL.emit_code) in
  let insts = Array.concat [[| Il.label (mk_fname fname) |];
                            Array.of_list prologue;
                            code;
                            Array.of_list epilogue] in
  let insts = fixup_allocation frame insts in
  ignore (pop_emitter());
  insts
  |> Array.to_list
  |> List.filter (fun o -> not(is_noop o))
end

and trans_frag (insts) = begin
  (* List.iter (fun i -> print_endline (IL.show_il i)) insts; *)
  let instsi = List.mapi (fun i x -> (x, i)) insts in
  let liveout = Liveness.build instsi in
  let igraph = Liveness.build_interference instsi liveout in
  (* Liveness.show_interference igraph; *)
  let colormap = RA.allocate instsi igraph in
  let instsgen = List.(insts
                       |> map (Codegen.codegen colormap)
                       |> concat) in
  instsgen
end

and trans_prog (ctx:ctx) (decs, stmt) (out: out_channel) = begin
  let ctx = add_function_decs decs ctx in
  let function_insts = decs |> List.map (trans_function_declaration ctx) in
  (* let _, gc_init_insts = trans_call ctx "init_gc_ctx" [] in *)
  let functiongen = List.map (trans_frag) (function_insts) |> List.concat in
  (* List.iter (fun i -> print_endline (Arm.string_of_inst' i)) functiongen; *)
  let frame = new_frame() in
  ignore (trans_call ctx frame "init_gc_ctx" []);
  let endenv = trans_stmt ctx frame stmt in
  ignore (trans_call ctx frame "destroy_gc_ctx" []);
  let prologue = (frame_prologue ctx frame) in
  let epilogue = (frame_epilogue endenv frame) in
  emit(IL.mov (F.reg_RV) (oper_imm 0));
  let code = ((emitter()).IL.emit_code) in
  let insts = Array.concat [Array.of_list prologue; code; Array.of_list epilogue] in
  let instsi = Array.mapi (fun i x -> (x, i)) insts in
  (* build CFG *)
  let liveout = Liveness.build (Array.to_list instsi) in
  let igraph = Liveness.build_interference (Array.to_list instsi) liveout in
  (* Liveness.show_interference igraph; *)
  let colormap = RA.allocate (Array.to_list instsi) igraph in
  let open Printf in
  let instsgen = (insts
                  |> Array.to_list
                  |> List.filter (fun o -> not(is_noop o))
                  |> List.map (Codegen.codegen colormap)
                  |> List.concat) in
  (* print out the generated code *)
  fprintf out ".data\n";
  List.(iter (function
      | STRING (label, string) -> pp_string out (label, string)
      | _ -> failwith "TODO"
  ) !frags;
     fprintf out ".text\n";
     fprintf out ".global main\n";
     iter (pp_inst out) (functiongen);
     fprintf out "main:\n";
     iter (pp_inst out) (instsgen));
  ()
end
