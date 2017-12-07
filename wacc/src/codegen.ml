(* Code-generation *)
module A = Arm;;
module IL = Il;;

let arm_op (op: IL.operand): A.operand = match op with
  | IL.OperReg r -> A.OperReg (r, None)
  | IL.OperImm i -> A.OperImm i

let arm_cond (cond: IL.cond): A.cond =
  let open IL in
  match cond with
  | GT -> A.GT
  | GE -> A.GE
  | LT -> A.LT
  | LE -> A.LE
  | EQ -> A.EQ
  | NE -> A.NE
  | VS -> A.VS

let arm_arith il =
let open IL in
match il with
  | ADD _ -> A.add
  | SUB _ -> A.sub
  | EOR _ -> A.eor
  | AND _ -> A.annd
  | ORR _ -> A.orr
  | EOR _ -> A.eor
  (*| MUL _ -> A.mul*)
  | _ -> failwith "not a valid instruction"

let arm_addr = function
  | IL.ADDR_LABEL l -> A.AddrLabel l
  | IL.ADDR_INDIRECT (base, offset) -> A.AddrIndirect (base, offset)

let is_reg = function
  | IL.OperReg _ -> true
  | _ -> false

let get_reg = function
  | IL.OperReg r -> r
  | _ -> failwith "not a reg"


let relocate (colormap) (i:A.inst'): A.inst' =
  let open Arm in
  let (ii, cond) = i in
  let (!) x = Hashtbl.find colormap x in
  let (!!) = function
    | OperImm i -> OperImm i
    | OperReg (r, s) -> OperReg (!r, s)
    | OperChar c -> OperChar c
  in
  let (!!!) a = match a with
    | AddrLabel l -> a
    | AddrIndirect (base, offset) -> AddrIndirect (!base, offset)  in
  let ir = match ii with
  | ADD  (dst, op1, op2) -> ADD (!dst, !op1, !!op2)
  | SUB  (dst, op1, op2) -> SUB (!dst, !op1, !!op2)
  | AND  (dst, op1, op2) -> AND (!dst, !op1, !!op2)
  | ORR  (dst, op1, op2) -> ORR (!dst, !op1, !!op2)
  | MUL  (dst, op1, op2) -> MUL (!dst, !op1, !op2)
  | SMULL (r0, r1, r2, r3) -> SMULL (!r0, !r1, !r2, !r1)
  | MOV  (r, op) -> MOV (!r, !!op)
  | CMP  (r, op) -> CMP (!r, !!op)
  | POP  [] -> POP (Arm.callee_saved_regs)
  | POP  rs -> POP (List.map (!) rs)
  | PUSH  [] -> PUSH (Arm.callee_saved_regs)
  | PUSH rs -> PUSH (List.map (!) rs)
  | LDR  (r, addr) -> LDR (!r, !!!addr)
  | LDRB (r, addr) -> LDRB (!r, !!!addr)
  | STR  (r, addr) -> STR (!r, !!!addr)
  | STRB (r, addr) -> STRB (!r, !!!addr)
  | EOR  (r0, r1, op) -> EOR (!r0, !r1, !!op)
  | _ -> ii in
  (ir, cond)


let codegen (colormap: (Temp.temp, Temp.temp) Hashtbl.t)
    (il: IL.il): A.inst' list =
  let open IL in
  let open Arm in
  let arm_inst = match il with
  | ADD   (t, op, op2) | SUB (t, op, op2)
  | AND   (t, op, op2) | ORR (t, op, op2) | EOR (t, op, op2) -> begin
      if is_reg op then begin
      let op2' = arm_op  op2 in
      let f = arm_arith il in
      [f (t) (get_reg op) op2'; bl ~cond:Arm.VS "wacc_throw_overflow_error"]
      end
      else
        let f = arm_arith il in
        [mov t (arm_op op);
         f (t) t (arm_op op2); bl ~cond:Arm.VS "wacc_throw_overflow_error"]
    end
  | MUL   (t, op, op2) -> begin
      if is_reg op then begin
        let op' = get_reg op in
        let op2' = get_reg op2 in
        [Arm.smull op' op2' op' op2';
         Arm.cmp op2' (Arm.OperReg (op', Some (ASR 31)));
         Arm.bl ~cond:NE "wacc_throw_overflow_error"]
      end
      else begin
        let op' = get_reg op in
        let op2' = get_reg op2 in
        [Arm.mov t (arm_op op);
         Arm.smull op' op2' op' op2';
         Arm.cmp op2' (Arm.OperReg (op', Some (ASR 31)));
         Arm.bl ~cond:NE "wacc_throw_overflow_error"]
      end
    end
  | LOAD  (size, t, addr) -> begin
      match size with
      | WORD -> [load  (t) (arm_addr addr)]
      | BYTE -> [loadb (t) (arm_addr addr)]
    end
  | STORE (size, t, addr) -> begin
      match size with
      | WORD -> [str  (t) (arm_addr addr)]
      | BYTE -> [strb (t) (arm_addr addr)]
    end
  | DIV _ -> assert false
  | JUMP   label -> [A.jump label]
  | CALL   label -> [A.bl label]
  | CMP   (cond, t, op, op2) -> begin
      let cond' = arm_cond cond in
      let complement_cond = match cond' with
        | EQ -> NE
        | LT -> GE
        | LE -> GT
        | GT -> LE
        | GE -> LT
        | NE -> EQ
        | VS -> VS
      in
      [cmp (get_reg op) (arm_op op2);
       mov ~cond:cond' t (Arm.OperImm 1);
       mov ~cond:complement_cond t (Arm.OperImm 0)]
    end
  | COMP  (t0, t1) -> begin
      let op2 = A.OperReg (t1, None) in
      [A.cmp (t0) op2]
    end
  | CBR   (t, br_then , br_else) -> begin
      [cmp (t) (A.OperImm(1)); jump ~cond:NE br_else]
    end
  | RET    t  -> [mov (reg_RV) (A.OperReg(t,None))]
  | LABEL  label -> [A.labels label]
  | NOOP -> [A.labels ""]
  | MOV (t, op) -> [mov (t) (arm_op op)]
  | PUSH temp_list -> [push temp_list]
  | POP temp_list ->  [pop  temp_list]
  | LTORG -> [(A.LTORG, None)]
  in
  List.map (relocate colormap) arm_inst
