(* See spec-120.3 page 12 *)
module Arm_Asm = struct
  type label = Temp.label
  and inst =
    | InstDp of inst_dp
    | InstMult of inst_mult
    | InstSdt of inst_sdt
    | InstBr of inst_br
    | InstSp of inst_sp
  and inst_dp
    | Dp of opcode * reg * operand2
  and inst_sdt
    | Sdt
  and inst_br
    | Br of opcode * label
  and inst_sp
    | Lsl of reg * reg
    | Halt
  and inst_mult
    | Mult of opcode
  and opcode =
  (* Data processing opcodes *)
    | Add
    | Sub
    | Rsb
    | And
    | Eor
    | Orr
    | Mov
    | Tst
    | Teq
    | Cmp
  (* Single Data Transfer opcodes *)
    | LDR
    | STR
  (* Multiply opcodes *)
    | Mul
    | Mla
  (* Branching opcodes *)
    | Beq
    | Bne
    | Bge
    | Blt
    | Bgt
    | Ble
    | B
  (* Special opcodes *)
    | Lsl
    | Andeq
  and operand2 =
    | Op2
  type reg = Reg of int
  type addr
  type shift
  type expression
end
