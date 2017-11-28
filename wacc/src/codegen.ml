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

let arm_arith = function
  | IL.ADD _ -> A.add
  | IL.SUB _ -> A.sub
  | _ -> failwith "TODO"

let arm_addr = function
  | IL.ADDR_LABEL l -> A.AddrLabel l
  | IL.ADDR_INDIRECT (base, offset) -> A.AddrIndirect (base, offset)

let is_reg = function
  | IL.OperReg _ -> true
  | _ -> false

let get_reg = function
  | IL.OperReg r -> r
  | _ -> failwith "not a reg"

let codegen (il: IL.il): A.inst' list =
  let arm_reg x = x in
  let open IL in
  match il with
  | ADD   (t, op, op2) | SUB (t, op, op2)
  | DIV   (t, op, op2) | MUL (t, op, op2)
  | AND   (t, op, op2) | ORR (t, op, op2) -> begin
      if is_reg op then begin
      let op2' = arm_op op2 in
      let r = get_reg op in
      let f = arm_arith il in
      [f (arm_reg t) (arm_reg r) op2']
      end
      else failwith "should not have imm operand"
    end
  | LOAD  (size, t, addr) -> begin
      match size with
      | WORD -> [A.load  (arm_reg t) (arm_addr addr)]
      | BYTE -> [A.loadb (arm_reg t) (arm_addr addr)]
    end
  | STORE (size, t, addr) -> begin
      match size with
      | WORD -> [A.str  (arm_reg t) (arm_addr addr)]
      | BYTE -> [A.strb (arm_reg t) (arm_addr addr)]
    end
  | JUMP   label -> [A.bl label]
  | CMP   (cond, t, op, op2) -> failwith "TODO"
  | COMP  (t0, t1) -> begin
      let op2 = A.OperReg (t1, None) in
      [A.cmp (arm_reg t0) op2]
    end
  | CBR   (t, br_then , br_else) -> begin
      (* TODO *)
      [A.bl br_then; A.bl br_else]
    end
  | RET    t  -> failwith "TODO"
  | LABEL  label -> [A.labels label]
  | NOOP -> [A.labels ""]
  | _ -> failwith "TODO"
