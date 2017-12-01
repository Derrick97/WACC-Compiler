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

let frame_size (frame:frame): int = (Array.fold_left (+) 0 (Array.map (fun x -> match x with
    | InFrame (t, sz) -> sz) frame.frame_locals))

let (frags: frag list ref) = ref []
let counter = ref 0
let strings = ref []

let is_pair = function
  | A.PairTy _ | A.PairTyy -> true
  | _ -> false

let is_string ty = match ty with
  | A.StringTy -> true
  | _ -> false

let is_char_array ty = match ty with
  | A.ArrayTy(A.CharTy) -> true
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

let trans_var (var: access): (temp * Il.il list) =
  let open Il in
  let t = allocate_temp() in
  match var with
  | InFrame (offset, sz) ->
      (match sz with
       | 4 -> t, [(load WORD t (ADDR_INDIRECT (Arm.reg_SP, offset)))]
       | 1 -> t, [(load BYTE t (ADDR_INDIRECT (Arm.reg_SP, offset)))]
       | _ -> assert false)
  | InReg r -> t, [Il.MOV (t, Il.OperReg r)]

let trans_assign
    (lv: access)
    (rv: temp): (il list) = begin
  let open IL in
  match lv with
  | InFrame (offset, sz) -> (
      match sz with
      | 4 -> [(store WORD rv (ADDR_INDIRECT (Arm.reg_SP, offset)))]
      | 1 -> [(store BYTE rv (ADDR_INDIRECT (Arm.reg_SP, offset)))]
      | _ -> assert false)
  | InReg (dst) -> [(mov dst (OperReg rv))]
end

let trans_noop: il list = []

let get_access env name = match Symbol.lookup name env with
  | E.VarEntry (t, Some acc) -> (t, acc)
  | _ -> invalid_arg "not an access"

let rec trans_exp ctx exp =
  let open Il in
  let (exp, pos) = exp in
  let tr = trans_exp ctx in
  let dst = allocate_temp() in
  begin match exp with
    | A.IdentExp       name -> begin
        let (t, acc) = get_access ctx name in
        trans_var acc
      end
    | A.ArrayIndexExp (ident, exps) -> begin
        let (t, acc) = get_access ctx ident in
        let size = if is_string t then 1 else 4 in
        let buffer = ref [] in
        let emit xs = buffer := !buffer @ xs in
        let index = allocate_temp() in
        emit([mov index (oper_imm 0)]); (* initializing index register *)
        let o = allocate_temp() in
        let sz = if not (is_string t) then 4 else 0 in (* for char arrays we do not store length *)
        let addr, insts = trans_var acc in
        emit(insts);
        let rec emit_addressing = (function
            | exp::other -> begin
                let index, insts = trans_exp ctx exp in
                emit(insts);
                emit(snd (trans_call ctx "wacc_check_array_bounds" [addr; index]));
                emit([mov o (oper_imm size);
                      mul index (oper_reg index) (oper_reg o); (* compute offset *)
                      add addr  (oper_reg addr)  (oper_reg index);
                      add addr (oper_reg addr) (oper_imm sz);
                      load WORD addr (addr_indirect addr 0)]);
                emit_addressing other
              end
            | [] -> ()) in
        emit_addressing exps;
        emit([mov dst (oper_reg addr)]);
        dst, !buffer
        end
    | A.LiteralExp    (lit) -> begin
        let open Il in
        let insts = (match lit with
            | A.LitInt i -> [(load WORD dst (ADDR_LABEL (string_of_int i)))]
            | A.LitString s -> begin
                let label = new_label() in
                frags := (STRING (label, s))::!frags;
                [(load WORD dst (ADDR_LABEL label))]
              end
            | A.LitBool b -> if b then
                  [(mov dst (OperImm 1))]
                else
                  [(mov dst (OperImm 0))]
            | A.LitChar c -> [(mov dst (OperImm (Char.code c)))] (* TODO imm char? *)
            | _ -> assert false) in
        dst, insts
      end
    | A.BinOpExp      ((lhs), binop, (rhs)) -> begin
        let (l, lhsi) = (trans_exp ctx lhs) in
        let (r, rhsi) = (trans_exp ctx rhs) in
        let (opl, opr) = (oper_reg l), (oper_reg r) in
        let inst = (match binop with
            | A.PlusOp   -> [add dst opl opr]
            | A.MinusOp  -> [sub dst opl opr]
            | A.TimesOp  -> [mul dst opl opr]
            | A.DivideOp -> [div dst opl opr]
            | A.AndOp ->    [and_ dst opl opr]
            | A.OrOp  ->    [or_ dst opl opr]
            | A.EqOp -> [sub dst opl opr; cmp Il.EQ dst (oper_reg dst) (oper_imm 0)]
            | A.NeOp -> [sub dst opl opr; cmp Il.NE dst (oper_reg dst) (oper_imm 0)]
            | A.GeOp -> [sub dst opl opr; cmp Il.GE dst (oper_reg dst) (oper_imm 0)]
            | A.GtOp -> [sub dst opl opr; cmp Il.GT dst (oper_reg dst) (oper_imm 0)]
            | A.LeOp -> [sub dst opl opr; cmp Il.LE dst (oper_reg dst) (oper_imm 0)]
            | A.LtOp -> [sub dst opl opr; cmp Il.LT dst (oper_reg dst) (oper_imm 0)]
            | A.ModOp -> invalid_arg "mod should be handled in frontend")
        in
        dst, (lhsi @ rhsi @ inst)
      end
    | A.UnOpExp       (A.NegOp, exp) -> begin
        (* FIXME a problem with the limitation of ARM architecture *)
        trans_exp ctx ((A.BinOpExp ((A.LiteralExp (A.LitInt 0), pos), A.MinusOp, exp), pos))
      end
    | A.UnOpExp       (unop, exp) -> begin
        let (t, expi) = trans_exp ctx exp in
        let insts = (match unop with
            | A.NotOp -> [eor dst (oper_reg dst) (oper_imm 1)]
            | A.NegOp -> [sub dst (oper_imm 0) (oper_reg t)]
            | A.LenOp | A.OrdOp | A.ChrOp
              -> failwith "should be desugared" ) in
        (dst, insts)
      end
    | A.CallExp       (fname, args) -> begin
        let buildin_func =
          ["wacc_len"; "wacc_ord"; "wacc_mod"; "wacc_chr"; "wacc_div"; "wacc_exit"] in
        let argsi = List.map (fun arge -> trans_exp ctx arge) args in
        let insts = List.concat (List.map snd argsi) in
        let argtemps = List.map fst argsi in
        let resultt, calli =
          if (List.length (List.filter (fun x -> x = fname) buildin_func) = 0)
          then
            trans_call ctx ("f_" ^ fname) argtemps
          else
            trans_call ctx (fname) argtemps
          in
        resultt, (insts @ calli)
      end
    | A.NewPairExp    (lval, exp) -> failwith "newpair should not happen"
    | A.FstExp         exp  -> begin
        let expt, expinsts = tr exp in
        let next = allocate_temp() in
        let _, insts = (trans_call ctx "wacc_check_pair_null" [expt]) in
        let insts = insts @ [load WORD next (addr_indirect expt  0);
                             load WORD dst  (addr_indirect next 0)] in
        dst, expinsts @ insts
      end
    | A.SndExp         exp  -> begin
        let expt, expinsts = tr exp in
        let next = allocate_temp() in
        let _, insts =  (trans_call ctx "wacc_check_pair_null" [expt]) in
        let insts = insts @ [load WORD next (addr_indirect expt 4);
                             load WORD dst  (addr_indirect next 0)] in
        dst, expinsts @ insts
      end
    | A.NullExp -> dst, [mov dst (oper_imm 0)]
  end

and trans_call (ctx: ctx)
    (fname: string)
    (args: temp list): (temp * Il.il list) = begin
  let open Il in
  let fname_label = new_namedlabel fname in
  let ilist = ref [] in
  let emit x = ilist := !ilist @ [x] in
  let result = allocate_temp() in
  let split_reg_stack_passed regs =
    let reg_passed = ref [] in
    let stack_passed = ref [] in
    List.iteri (fun i x -> if i <= 3
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
  result, !ilist @ [call fname_label] @
  [mov result (oper_reg F.reg_RV)] @
  [pop F.caller_saved_regs] @
  (List.map (fun x -> (Il.POP [x])) (stack_passed));
end

let rec trans_ifelse (cond: temp) (t: il list) (f: il list) = begin
  let open Il in
  let true_l = new_label ~prefix:"if_then" () in
  let false_l = new_label ~prefix:"if_else" () in
  let end_l = new_label ~prefix:"if_end" () in
  [cbr cond true_l false_l] @
  [label true_l] @
   t             @
  [jump  end_l]  @
  [label false_l]@
   f             @
  [label end_l]
end

and addr_of_exp (env) (e: A.exp): (Il.addr * il list) =
  let open A in
  let open Il in
  let (exp', pos) = e in
  match exp' with
  | ArrayIndexExp (name, [exp]) -> begin
      let t, acc = get_access env name in
      let size = if is_string t || is_char_array t then 1 else size_of_type t in
      let o = allocate_temp() in
      let index, insts = trans_exp env exp in
      let skip_length_inst = if is_string t then [] else
          [add index (oper_reg index) (oper_imm 4)] in
      let addr, var_insts = trans_var acc in
      let _, callinsts = trans_call env "wacc_check_array_bounds" [addr; index] in
      let insts = insts @ var_insts
                  @ callinsts
                  @ [mov o (oper_imm size);
                     mul index (oper_reg index) (oper_reg o)]
                  @ skip_length_inst @
                  [add addr (oper_reg addr) (oper_reg index)] in
      Il.ADDR_INDIRECT (addr, 0), insts
    end
  | ArrayIndexExp _ -> assert false
  | IdentExp (name) -> begin
      let VarEntry (ty, Some (InFrame (offset, sz) as acc)) (* FIXME *)
        = (Symbol.lookup name env) in
      let dst, insts = trans_var acc in
      (addr_indirect Arm.reg_SP offset), insts
    end
  | FstExp (exp) -> begin
      let dst, insts = trans_exp env exp in
      let _, insts' = trans_call env "wacc_check_pair_null" [dst] in
      let insts = insts @ insts' in
       (addr_indirect dst 0), insts @ [load WORD dst (addr_indirect dst 0)]
    end
  | SndExp (exp) -> begin
      let dst, insts = trans_exp env exp  in
      let _, insts' = trans_call env "wacc_check_pair_null" [dst] in
      let insts = insts @ insts' in
      (addr_indirect dst 0), insts @ [load WORD dst (addr_indirect dst 4)]
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

let rec trans_stmt env frame stmt = begin
  let open Il in
  let tr = trans_exp env in
  let (stmt, _) = stmt in
  let get_access = get_access env in
  match stmt with
  | A.SeqStmt (stmt, stmtlist) -> begin
      let exp,  env' = trans_stmt env frame stmt in
      let exp', env'' = (trans_stmt env' frame stmtlist) in
      (exp @ exp', env'')
    end
  | A.PrintStmt (newline, exp) -> begin
      let (v, insts) = trans_exp env exp in
      let expt = exp_type env exp in
      let _, callinsts = trans_call env ("wacc_print_" ^ (string_of_ty expt)) [v] in (* TODO add type for print *)
      let callinsts' =(if newline then (snd (trans_call env "wacc_println" [])) else []) in
      insts @ callinsts @ callinsts', env
    end
  | A.AssignStmt   ((A.IdentExp (name), _), rhs) -> begin
      let (ty, acc) = get_access name in
      let rhst, rhsi = tr rhs in
      let ass = (trans_assign acc rhst) in
      rhsi @ ass, env
    end
  | A.AssignStmt   (lhs, rhs) -> begin
      let rhst, rinsts = trans_exp env rhs in
      let ADDR_INDIRECT (base, offset) as laddr, linsts = addr_of_exp env lhs in
      let other = allocate_temp() in
      let check_null_insts = [] in
      (* let check_null_insts = [add other (oper_reg base) (oper_imm offset)]
       *                        @ trans_call env "wacc_check_pair_null" [other] in *)
      let insts = rinsts @ linsts @ check_null_insts in
      let insts = insts @ (if size_of_type (Semantic.check_exp env rhs) = 4 then
          [store WORD rhst laddr] else [store BYTE rhst laddr]) in (* TODO need to handle storeb *)
      (insts, env)
    end
  | A.VarDeclStmt  (A.ArrayTy ty, name, (A.LiteralExp(A.LitArray elements), _)) -> begin
      let insts_list = ref [] in
      let emit x = insts_list := !insts_list @ x in
      let local_var = allocate_local frame 4 in
      let env' = Symbol.insert name (VarEntry (A.ArrayTy ty, Some local_var)) env in
      let array_length = List.length elements in
      let element_size = 4 in
      let dst = allocate_temp() in
      let next = allocate_temp() in
      emit([mov dst (oper_imm (array_length * element_size + 4))]);
      let addr_reg, alloc_insts = trans_call env "malloc" [dst] in
      emit(alloc_insts);
      List.iteri (fun i item -> begin
            let t, is = tr (item) in
            emit(is);
            emit([store WORD t (addr_indirect addr_reg (element_size * i + 4))]);
          end) elements;
      emit([mov next (oper_imm array_length);
            (store WORD next (addr_indirect addr_reg 0))]);
      emit(trans_assign local_var addr_reg);
      !insts_list, env'
    end
  | A.VarDeclStmt (ty, name, (A.NewPairExp (exp, exp'),_)) -> begin
      let local_var = allocate_local frame 4 in
      let env' = Symbol.insert name (VarEntry (ty, Some local_var)) env in
      let sizet = allocate_temp() in
      let fstt, fstinsts = tr exp in
      let sndt, sndinsts = tr exp' in
      let exp_ty = exp_type env exp in
      let exp'_ty = exp_type env exp' in
      let r0, r1 = allocate_temp(), allocate_temp() in
      let pair_addrt, pair_addr_alloc_insts = trans_call  env "malloc" [sizet] in
      let fst_addrt, fst_alloc_insts = trans_call env "malloc" [r0] in
      let snd_addrt, snd_alloc_insts = trans_call env "malloc" [r1] in
      fstinsts @ sndinsts @
      (* allocate for pair, each pair is represented with 4 * 2
         bytes of addresses on the heap *)
      (* TODO: fix passing return valies back *)
      [mov sizet (oper_imm (4 * 2))] @
       pair_addr_alloc_insts @
      [store WORD pair_addrt (addr_indirect (F.reg_SP) 0)] @
      (* fst allocation *)
      [mov r0 (oper_imm (size_of_type exp_ty))] @
       fst_alloc_insts @
      [store WORD fst_addrt (addr_indirect pair_addrt 0);
       store WORD fstt (addr_indirect fst_addrt 0)] (* fixme handle byte store *) @
      (* snd allocation *)
      [mov r1 (oper_imm (size_of_type exp'_ty))] @
       snd_alloc_insts @
      [store WORD snd_addrt  (addr_indirect pair_addrt 4);
       store WORD sndt (addr_indirect snd_addrt 0)]
      @ trans_assign local_var pair_addrt, env'
    end
  | A.VarDeclStmt  (ty, name, exp) -> begin
      let size = size_of_type ty in
      let local_var = allocate_local frame size in
      let env' = Symbol.insert name (VarEntry (ty, Some local_var)) env in
      let expt, expi = tr exp in
      let assigni = trans_assign (local_var) expt in
      expi @ assigni, env'
    end
  | A.SkipStmt       -> trans_noop, env
  | A.CallStmt (exp) -> snd (trans_exp env exp), env
  | A.ReadStmt (exp) -> begin
      let addr, insts = addr_of_exp env exp in
      let ty = exp_type env exp in
      let readt, readinsts = trans_call env ("wacc_read_" ^ (string_of_ty ty)) [] in
      let readinsts = readinsts @ (if ty = A.CharTy then
                                     [store BYTE readt addr]
                                   else [store WORD readt addr]) in
      insts @ readinsts, env
    end
  | A.FreeStmt     (exp) -> begin
      let expt, expi = tr exp in
      let _, ci = trans_call env "wacc_free" [expt] in
      expi @ ci, env
    end
  | A.IfStmt       (cond, then_exp, else_exp) -> begin
      let env' = Symbol.new_scope env in
      let condt, condi = trans_exp env' cond in
      let theni,_ = trans_stmt env' frame then_exp in
      let elsei,_ = trans_stmt env' frame else_exp in
      let stmti = trans_ifelse condt theni elsei in
      condi @ stmti, env
    end
  | A.WhileStmt    (cond, body_stmt) -> begin
      let env' = Symbol.new_scope env in
      let condt, condi = tr cond in
      let bodyi, _ = trans_stmt env' frame body_stmt in
      let while_cond_l = new_label ~prefix:"while_cond" () in
      let while_body_l = new_label ~prefix:"while_body" () in
      let while_end_l =  new_label ~prefix:"while_done" () in
      [label (while_cond_l)] @ condi @
      [cbr condt while_body_l while_end_l] @
      [label (while_body_l)] @ bodyi @
      [jump (while_cond_l);
       label while_end_l], env
    end
  | A.ExitStmt (exp) -> begin
      failwith "TODO should be desugared"
    end
  | A.RetStmt (exp) -> begin
      let open F in
      let open Il in
      let expt, expi = tr exp in
      let retinsts =  [mov (reg_RV) (oper_reg expt);
                       add reg_SP (oper_reg reg_SP) (oper_imm (frame_size frame));
                       pop [reg_PC]] in
      expi @ retinsts, env
    end
  | A.BlockStmt (body) -> begin
      let env' = Symbol.new_scope env in
      let insts, _ = trans_stmt env' frame body in
      insts, env
    end
end

and pp_string out (l, s): unit =
  let open Printf in
  fprintf out "%s" (sprintf "%s:\n\t.ascii \"%s\0\"\n" l (String.escaped s))

and pp_inst out (i: Arm.inst') =
  Printf.fprintf out "%s\n" (Arm.string_of_inst' i)

and frame_prologue (frame: frame): il list = begin
  let open IL in
  let local_size = frame_size frame in
  let allocate_insts = if (local_size > 0) then
      [sub F.reg_SP (oper_reg F.reg_SP) (oper_imm local_size)]
    else [] in
   [push [F.reg_LR]] @ allocate_insts
end

and frame_epilogue (frame: frame): il list = begin
  let open IL in
  let local_size = frame_size frame in
  let deallocate_insts = if (local_size > 0) then
      [add F.reg_SP (oper_reg F.reg_SP) (oper_imm local_size)]
    else [] in
  deallocate_insts @
  [pop [F.reg_PC]]
end

and fixup_allocation frame insts =
  let open Il in
  let localsize = frame_size frame in
  List.map (fun i -> match i with
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
  let insts, _ = trans_stmt !env' frame body in
  let insts = (frame_prologue frame) @ insts @ (frame_epilogue frame) in
  (* view shift of local variables *)
  let insts = (fixup_allocation frame insts) in
  [Il.label ("f_"^fname)] @ insts
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
  let functiongen = List.map (trans_frag) (function_insts) |> List.concat in
  let frame = new_frame() in
  let insts, _ = trans_stmt ctx frame stmt in
  (* List.iter (fun i -> print_endline (IL.show_il i)) insts; *)
  let insts = (frame_prologue frame) @ insts @ [IL.mov (F.reg_RV) (oper_imm 0)] @ (frame_epilogue frame) in
  let instsi = List.mapi (fun i x -> (x, i)) insts in
  (* build CFG *)
  let liveout = Liveness.build instsi in
  let igraph = Liveness.build_interference instsi liveout in
  (* Liveness.show_interference igraph; *)
  let colormap = RA.allocate instsi igraph in
  let open Printf in
  (* print_endline "Allocation";
   * Hashtbl.iter (fun k v -> begin
   *       print_endline (Printf.sprintf "%s: %s" k v)
   *     end) colormap;
   * print_endline "End of allocation"; *)
  let instsgen = List.(insts
                      |> map (Codegen.codegen colormap)
                      |> concat) in
  (* print out the generated code *)
  fprintf out ".data\n";
  List.(iter (function
      | STRING (label, string) -> pp_string out (label, string)
      | _ -> failwith "TODO"
  ) !frags;
     fprintf out ".text\n";
     fprintf out ".global main\n";
     List.iter (pp_inst out) (functiongen);
     fprintf out "main:\n";
    iter (pp_inst out) (instsgen);
  ); insts
end
