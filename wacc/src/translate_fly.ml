module S = Symbol;;
module A = Ast;;
module T = Tree;;

type frag = unit
type size = int

and  access =
  | InFrame of int * size
  | InReg of Temp.temp * size

and exp =
  | Imm of int * size
  | InAccess of access
  | Label of string

type frame = {
  mutable frame_counter: int;
  mutable frame_locals: access array;
}

let ex_temp (exp) = match exp with
  | InAccess (InReg (t, size)) -> t
  | _ -> assert false

let operand_of_exp (exp: exp): Arm.operand
  = failwith "TODO"

let new_frame frame = {
    frame_counter = 0;
    frame_locals = [| |];
}

let emit (inst: Arm.inst') =
  let inst, _ = inst in
  print_endline (Arm.string_of_inst inst)


let temp_counter = ref 0

let new_temp (): Temp.temp =
  let i = !temp_counter in
  temp_counter := i + 1;
  i

let new_label (): string = "l"
let new_namedlabel name = name

let trans_call (fname: string)
    (args: exp list): exp = begin
  let fname_label = new_namedlabel fname in
  emit (Arm.BL(fname_label), None);
  InAccess(InReg (Arm.reg_RV, 4))
end

let trans_unop  (op: A.unop) (exp: exp): exp = match op with
  | A.NotOp -> begin
      trans_call "wacc_len" [exp]
    end
  | A.NegOp -> begin
      let exp' = ex_temp exp in
      let t = new_temp () in
      emit(Arm.MOV(t, Arm.OperImm 0), None);
      emit(Arm.SUB(exp', t, (Arm.OperReg exp')), None);
      exp
    end
  | A.LenOp -> trans_call "wacc_len" [exp]
  | A.OrdOp -> trans_call "wacc_ord" [exp]
  | A.ChrOp -> trans_call "wacc_chr" [exp]

let trans_binop  (op: A.binop) (lhs: exp) (rhs: exp):exp =
  let lhs' = ex_temp lhs in
  let rhs' = ex_temp rhs in
  match op with
  | A.PlusOp -> emit(Arm.ADD(lhs', lhs', (Arm.OperReg rhs')), None); lhs
  | A.MinusOp -> emit(Arm.SUB(lhs', lhs', (Arm.OperReg rhs')), None); lhs
  | A.TimesOp -> emit(Arm.MUL(lhs', lhs', rhs'), None); lhs
  | A.DivideOp -> failwith "Notimplemented"
  | A.AndOp -> emit(Arm.AND (lhs', lhs', (Arm.OperReg rhs')), None); lhs
  | A.OrOp  -> emit(Arm.ORR (lhs', lhs', (Arm.OperReg rhs')), None); lhs
  | A.ModOp -> trans_call "wacc_mod" [lhs; rhs]
  | A.GeOp -> begin
      let t = new_temp () in
      emit(Arm.CMP (lhs', operand_of_exp rhs), None);
      emit(Arm.MOV(t, Arm.OperImm 1), Some Arm.GE);
      emit(Arm.MOV(t, Arm.OperImm 0), Some Arm.LT);
      InAccess(InReg (t, 1))
    end
  | A.GtOp -> begin
      let t = new_temp () in
      emit(Arm.CMP (lhs', operand_of_exp rhs), None);
      emit(Arm.MOV(t, Arm.OperImm 1), Some Arm.GT);
      emit(Arm.MOV(t, Arm.OperImm 0), Some Arm.LE);
      InAccess(InReg (t, 1))
    end
  | A.LeOp -> begin
      let t = new_temp () in
      emit(Arm.CMP (lhs', operand_of_exp rhs), None);
      emit(Arm.MOV(t, Arm.OperImm 1), Some Arm.LE);
      emit(Arm.MOV(t, Arm.OperImm 0), Some Arm.GT);
      InAccess(InReg (t, 1))
    end
  | A.LtOp -> begin
      let t = new_temp () in
      emit(Arm.CMP (lhs', operand_of_exp rhs), None);
      emit(Arm.MOV(t, Arm.OperImm 1), Some Arm.LT);
      emit(Arm.MOV(t, Arm.OperImm 0), Some Arm.GE);
      InAccess(InReg (t, 1))
    end
  | A.EqOp -> begin
      let t = new_temp () in
      emit(Arm.CMP (lhs', operand_of_exp rhs), None);
      emit(Arm.MOV(t, Arm.OperImm 1), Some Arm.EQ);
      emit(Arm.MOV(t, Arm.OperImm 0), Some Arm.NE);
      InAccess(InReg (t, 1))
    end
  | A.NeOp -> begin
      let t = new_temp () in
      emit(Arm.CMP (lhs', operand_of_exp rhs), None);
      emit(Arm.MOV(t, Arm.OperImm 1), Some Arm.NE);
      emit(Arm.MOV(t, Arm.OperImm 0), Some Arm.EQ);
      InAccess(InReg (t, 1))
    end

let trans_lit    (l: A.literal) = match l with
  | _ -> assert false

let trans_ifelse (cond: exp) (t: exp) (f: exp) = begin
  let true_l = new_namedlabel "if_then" in
  let false_l = new_namedlabel "if_else" in
  let end_l = new_namedlabel "if_end" in
  let cond_t = ex_temp cond in
  emit(Arm.CMP(cond_t, Arm.OperImm 1), None);
  emit(Arm.B(true_l),                  Some Arm.EQ);
  emit(Arm.B(false_l),                 Some Arm.NE);
  emit(Arm.B(end_l),                   None)
end

let trans_var    (var: access): exp = match var with
  | InFrame (offset, sz) ->
    let t = new_temp () in
    emit(Arm.LDR(t, Arm.AddrIndirect (Arm.reg_SP, offset)), None);
    InAccess(InReg(t, sz))
  | _ -> assert false

let trans_assign (lv: exp) (rv: exp) = begin
  let InAccess(InFrame (offset, sz)) = lv in
  let InAccess(InReg(t, sz)) = rv in
  emit(Arm.STR(t, Arm.AddrIndirect (Arm.reg_SP, offset)), None);
  InAccess(InReg(t, sz))
end

let trans_array  (var: access) (indices: exp list)
  = failwith "TODO"

let trans_while  (cond: exp) (body: exp) = begin
  let while_cond_l = new_namedlabel "while_cond" in
  let while_end_l =  new_namedlabel "while_done" in
  let cond_t = ex_temp cond in
  emit(Arm.CMP(cond_t, Arm.OperImm 1), None);
  emit(Arm.B(while_cond_l),  Some Arm.EQ);
  emit(Arm.B(while_end_l), None)
end

let trans_seq    (first: exp) (follow: exp) = begin
  failwith "TODO"
end

let allocate_local (frame: frame) (size: size) =
  let i = frame.frame_counter in
  frame.frame_counter <- i + 1;
  InFrame (i, size)

let trans_noop: exp = failwith "TODO"

let access_of_exp (exp: exp): access = failwith "TODO"

let function_prologue (frame: frame) (args: access list): unit = failwith "TODO"
let function_epilogue (frame: frame) = failwith "TODO"
