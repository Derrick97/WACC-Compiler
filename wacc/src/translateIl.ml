module IL = Il;;
module A = Ast_v2;;
module E = Env;;
module S = Semantic;;
module F = Arm;;
module RA = Register_allocator;;
open Env;;

type frag = string list
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
  | _ -> failwith "not an access"

let rec trans_exp ctx exp =
  let open Il in
  let (exp, _) = exp in
  let tr = trans_exp ctx in
  let dst = allocate_temp() in
  begin match exp with
    | A.IdentExp       name -> begin
        let (t, acc) = get_access ctx name in
        trans_var acc
      end
    | A.ArrayIndexExp (ident, exps) -> failwith "TODO"
    | A.LiteralExp    (lit) -> begin
        let open Il in
        let insts = (match lit with
            | A.LitInt i -> [(load WORD dst (ADDR_LABEL (string_of_int i)))]
            | A.LitString s -> begin
                let label = new_label() in
                strings := (label, s)::!strings;
                [(load WORD dst (ADDR_LABEL label))]
              end
            | A.LitBool b -> if b then
                  [(mov dst (OperImm 1))]
                else
                  [(mov dst (OperImm 0))]
            | A.LitChar c -> [(mov dst (OperImm (Char.code c)))] (* TODO imm char? *)
            |  _ -> assert false) in
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
            | A.ModOp -> failwith "mod should be handled in frontend"
            | _ -> failwith "TODO")
        in
        dst, (lhsi @ rhsi @ inst)
      end
    | A.UnOpExp       (unop, exp) -> begin
        let (t, expi) = trans_exp ctx exp in
        let opt = oper_reg t in
        let insts = (match unop with
            | A.NotOp -> failwith "TODO"
            | A.NegOp -> [SUB(dst, OperImm 0, opt)]
            | A.LenOp | A.OrdOp | A.ChrOp | A.IncOp
              -> failwith "should be desugared" ) in
        (dst, insts)
      end
    | A.CallExp       (fname, args) -> begin
        let argsi = List.map (fun arge -> trans_exp ctx arge) args in
        let insts = List.concat (List.map snd argsi) in
        let resultt = allocate_temp() in
        let argtemps = List.map fst argsi in
        let calli = trans_call ctx fname argtemps in
        resultt, (insts @ calli)
      end
    | A.NewPairExp    (lval, exp) -> failwith "TODO"
    | A.FstExp         exp  -> begin
        let expt, expinsts = tr exp in
        let next = allocate_temp() in
        let insts =  (trans_call ctx "wacc_check_pair_null" [dst]) @
                     [load WORD next (addr_indirect dst  0);
                      load WORD dst  (addr_indirect next 0)] in
        dst, expinsts @ insts
      end
    | A.SndExp         exp  -> begin
        let expt, expinsts = tr exp in
        let next = allocate_temp() in
        let insts =  (trans_call ctx "wacc_check_pair_null" [dst]) @
                     [load WORD next (addr_indirect dst  4);
                      load WORD dst  (addr_indirect next 0)] in
        dst, expinsts @ insts
      end
    | A.NullExp -> dst, [mov dst (oper_imm 0)]
  end

and trans_call (ctx: ctx)
    (fname: string)
    (args: temp list): (Il.il list) = begin
  let open Il in
  let fname_label = new_namedlabel fname in
  let ilist = ref [] in
  let emit x = ilist := !ilist @ [x] in
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
  !ilist @ [jump fname_label] @
  [(pop F.caller_saved_regs)] @
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
      let insts = insts @ var_insts
                  @ trans_call env "wacc_check_array_bounds" [addr; index]
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
      let dst = allocate_temp() in
      let next, insts = trans_exp env exp in
      let insts = insts @ trans_call env "wacc_check_pair_null" [dst] in
       (addr_indirect next 0), insts @ [load WORD next (addr_indirect dst 0)]
    end
  | SndExp (exp) -> begin
      let dst = allocate_temp() in
      let next, insts = trans_exp env exp  in
      let insts = insts @ trans_call env "wacc_check_pair_null" [dst] in
      (addr_indirect next 0), insts @ [load WORD next (addr_indirect dst 4)]
    end
  | _ -> invalid_arg "Not an lvalue"

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
      let callinsts = trans_call env "wacc_print" [v] in (* TODO add type for print *)
      insts @ callinsts, env
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
      let check_null_insts = [add other (oper_reg base) (oper_imm offset)]
                             @ trans_call env "wacc_check_pair_null" [other] in
      let insts = rinsts @ linsts @ check_null_insts in
      let insts = insts @ (if size_of_type (Semantic.check_exp env rhs) = 4 then
          [store WORD rhst laddr] else [store BYTE rhst laddr]) in (* TODO need to handle storeb *)
      (insts, env)
    end
  | A.VarDeclStmt  (A.ArrayTy ty, name, (A.LiteralExp(A.LitArray elements),_)) -> begin
      let array_length = List.length elements in
      let dst = allocate_temp() in
      let addr_reg = dst in
      let next = allocate_temp() in
      let element_size = 4 in
      let local_var = allocate_local frame 4 in
      let env' = Symbol.insert name (VarEntry (A.ArrayTy ty, Some local_var)) env in
      let insts = [(mov dst (oper_imm (element_size * array_length + 4)))] @
                   trans_call env (*~result:dst*) "malloc" [dst] @
                  (trans_assign local_var dst) (* holds address to the heap allocated array *) in
      (* FIXME *)
                  (* @ List.concat (List.mapi (fun i e -> (translate_exp env e rest)
                   *                                      @ [STR (next, AddrIndirect (addr_reg, size_offset + element_size * i)), None]) elements) in *)
      let insts = insts @ [mov next (oper_imm array_length);
                           store WORD next (addr_indirect addr_reg 0)] in
      insts, env'
    end
  | A.VarDeclStmt (ty, name, (A.NewPairExp (exp, exp'),_)) -> begin
      let local_var = allocate_local frame 4 in
      let env' = Symbol.insert name (VarEntry (ty, Some local_var)) env in
      let pair_addrt = allocate_temp() in
      let fstt, fstinsts = tr exp in
      let sndt, sndinsts = tr exp in
      let exp_ty = Semantic.check_exp env exp in
      let exp'_ty = Semantic.check_exp env exp in
      let r0, r1, tmp = allocate_temp(), allocate_temp(), allocate_temp() in
      fstinsts @ sndinsts @
      (* allocate for pair, each pair is represented with 4 * 2
         bytes of addresses on the heap *)
      (* TODO: fix passing return valies back *)
      [mov pair_addrt (oper_imm (4 * 2))] @
       trans_call (* ~result:pair_addr *) env "malloc" [pair_addrt] @
      (* fst allocation *)
      [mov r0 (oper_imm (size_of_type exp_ty))] @
       trans_call (* ~result:tmp *) env "malloc" [r0] @
      [store WORD tmp  (addr_indirect pair_addrt 0);
       store WORD fstt (addr_indirect tmp 0)] (* fixme handle byte store *) @
      (* snd allocation *)
      [mov r1 (oper_imm (size_of_type exp'_ty))] @
       trans_call (* ~result:tmp *) env "malloc" [r1] @
      [store WORD tmp  (addr_indirect pair_addrt 4);
       store WORD sndt (addr_indirect tmp 0)]
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
  | A.IfStmt       (cond, then_exp, else_exp) -> begin
      let env' = Symbol.new_scope env in
      let condt, condi = trans_exp env' cond in
      let theni,_ = trans_stmt env' frame then_exp in
      let elsei,_ = trans_stmt env' frame else_exp in
      let stmti = trans_ifelse condt theni elsei in
      stmti, env
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
      [jump while_end_l;
       label (while_body_l)] @ bodyi @
      [jump (while_cond_l);
       label while_end_l], env
    end
  | A.ExitStmt (exp) -> failwith "exit should be desugared"
  | A.SideEffectStmt _ | A.TwoArgsSideEffectStmt _ -> failwith "Not supported"
  | _ -> failwith "TODO other statements"
end

and trans_prog (ctx:ctx) (decs, stmt) = begin
  let frame = new_frame() in
  let insts, _ = trans_stmt ctx frame stmt in
  List.iter (fun i -> print_endline (Il.show_il i)) insts;
  (* build CFG *)
  let liveout = Liveness.build insts in
  let igraph = Liveness.build_interference insts liveout in
  (* Liveness.show_interference igraph; *)
  let colormap = RA.allocate insts igraph in
  print_endline "Allocation";
  Hashtbl.iter (fun k v -> begin
        print_endline (Printf.sprintf "%s: %s" k v)
      end) colormap;
  insts;
  let instsgen = List.map (Codegen.codegen colormap) insts in
  List.iter (fun i -> print_endline (Arm.string_of_inst' i )) (List.concat (instsgen));
  insts;
end
