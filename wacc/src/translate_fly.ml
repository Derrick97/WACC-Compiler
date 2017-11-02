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
  let instList =
  match op with
  | A.PlusOp -> ([Arm.ADD(lhs', lhs', (Arm.OperReg rhs'))],InAccess(InReg(lhs',1)))
  | A.MinusOp -> ([Arm.SUB(lhs', lhs', (Arm.OperReg rhs'))],InAccess(InReg(lhs',1)))
  | A.TimesOp -> ([Arm.MUL(lhs', lhs', rhs')],InAccess(InReg(lhs',1)))
  | A.DivideOp -> failwith "Notimplemented"
  | A.AndOp -> ([Arm.AND (lhs', lhs', (Arm.OperReg rhs'))],InAccess(InReg(lhs',1)))
  | A.OrOp  -> ([Arm.ORR (lhs', lhs', (Arm.OperReg rhs'))],InAccess(InReg(lhs',1)))
  | A.ModOp -> trans_call "wacc_mod" [lhs; rhs]
  | A.GeOp ->
      let t = new_temp () in
      ([Arm.CMP (lhs', operand_of_exp rhs);
        Arm.MOV(t, Arm.OperImm 1);Arm.GE;
        Arm.MOV(t, Arm.OperImm 0);Arm.LT],InAccess(InReg(t,1)))

  | A.GtOp ->
      let t = new_temp () in
      ([Arm.CMP (lhs', operand_of_exp rhs);
        Arm.MOV(t, Arm.OperImm 1); Arm.GT;
        Arm.MOV(t, Arm.OperImm 0); Arm.LE],InAccess(InReg(t,1)))

  | A.LeOp ->
      let t = new_temp () in
      ([Arm.CMP (lhs', operand_of_exp rhs);
        Arm.MOV(t, Arm.OperImm 1); Arm.LE;
        Arm.MOV(t, Arm.OperImm 0); Arm.GT],InAccess(InReg(t,1)))

  | A.LtOp -> begin
      let t = new_temp () in
      ([Arm.CMP (lhs', operand_of_exp rhs);
        Arm.MOV(t, Arm.OperImm 1); Arm.LT;
        Arm.MOV(t, Arm.OperImm 0); Arm.GE],InAccess(InReg(t,1)))

  | A.EqOp -> begin
      let t = new_temp () in
      ([Arm.CMP (lhs', operand_of_exp rhs);
        Arm.MOV(t, Arm.OperImm 1); Arm.EQ;
        Arm.MOV(t, Arm.OperImm 0); Arm.NE],InAccess(InReg(t,1)))

  | A.NeOp -> begin
      let t = new_temp () in
      ([Arm.CMP (lhs', operand_of_exp rhs);
        Arm.MOV(t, Arm.OperImm 1); Arm.NE;
        Arm.MOV(t, Arm.OperImm 0); Arm.EQ],InAccess(InReg(t,1)))

let trans_lit    (l: A.literal) = match l with
  | _ -> assert false

let trans_ifelse (cond: exp) (t: exp) (f: exp) = begin
  let true_l = new_namedlabel "if_then" in
  let false_l = new_namedlabel "if_else" in
  let end_l = new_namedlabel "if_end" in
  let cond_t = ex_temp cond in
  [Arm.CMP(cond_t, Arm.OperImm 1);
   Arm.B(true_l); Arm.EQ ;
   Arm.B(false_l); Arm.NE;
   Arm.B(end_l)]
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
  ([Arm.STR(t, Arm.AddrIndirect (Arm.reg_SP, offset))],
  InAccess(InReg(t, sz)))
end

let trans_array  (var: access) (indices: exp list)
  = failwith "TODO"

let trans_while  (cond: exp) (body: exp) = begin
  let while_cond_l = new_namedlabel "while_cond" in
  let while_end_l =  new_namedlabel "while_done" in
  let cond_t = ex_temp cond in
  [Arm.CMP(cond_t, Arm.OperImm 1);
   Arm.B(while_cond_l); Arm.EQ;
   Arm.B(while_end_l)]
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
