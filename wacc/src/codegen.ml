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


  let counter = ref 0
  let cond_label ?(prefix="L") (): string =
    let i = !counter in
    counter := !counter + 1;
    prefix ^ (string_of_int i)

let arm_arith il =
let open IL in
match il with
  | ADD _ -> A.add
  | SUB _ -> A.sub
  | EOR _ -> A.eor
  | AND _ -> A.annd
  | ORR _ -> A.orr
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

let codegen (colormap: (Temp.temp, Temp.temp) Hashtbl.t)
    (il: IL.il): A.inst' list =
  let arm_reg x = x in
  let open IL in
  let open Arm in
  match il with
  | ADD   (t, op, op2) | SUB (t, op, op2)
  | DIV   (t, op, op2)
  | MUL (t, op, op2)
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
      | WORD -> [load  (arm_reg t) (arm_addr addr)]
      | BYTE -> [loadb (arm_reg t) (arm_addr addr)]
    end
  | STORE (size, t, addr) -> begin
      match size with
      | WORD -> [str  (arm_reg t) (arm_addr addr)]
      | BYTE -> [strb (arm_reg t) (arm_addr addr)]
    end
  | JUMP   label -> [A.bl label]
  | CMP   (cond, t, op, op2) -> (*[cmp (arm_reg op) (arm_reg op2); ]*) failwith "TODO CMP"
  | COMP  (t0, t1) -> begin
      let op2 = A.OperReg (t1, None) in
      [A.cmp (arm_reg t0) op2]
    end
  | CBR   (t, br_then , br_else) -> begin
      let br_end = cond_label ~prefix: "if_end" () in
      [cmp (arm_reg t) (A.OperImm(1)); jump ~cond:NE br_else; A.labels (cond_label ~prefix: "if_then" ());
      jump br_end; labels (cond_label ~prefix: "if_else" ()); labels br_end]
    end
  | RET    t  -> [mov (arm_reg reg_RV) (A.OperReg(t,None))]
  | LABEL  label -> [A.labels label]
  | NOOP -> [A.labels ""]
  | MOV (t, op) -> [mov (arm_reg t) (arm_op op)]
  | PUSH temp_list -> [push temp_list]
  | POP temp_list -> [pop temp_list]
  | _ -> failwith "TODO"
