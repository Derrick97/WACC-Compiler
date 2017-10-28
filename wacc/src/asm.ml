type inst =
  | Inst_oper of oper
  | Inst_mov of mov
  | Inst_label of string
  | Inst_jump of string
and oper =  { oper_dst: access list;
              oper_op: opcode;
              oper_src: access list; }
and mov =   { mov_src: access;
              mov_dst: access }
and opcode =
  (* Data processing opcodes *)
  | ADD
  | SUB
  | RSB
  | AND
  | EOR
  | ORR
  | TST
  | TEQ
  | CMP
  (* Single Data Transfer opcodes *)
  | LDR
  | STR
  (* Multiply opcodes *)
  | MUL
  | MLA
  (* Branching opcodes *)
  | BEQ
  | BNE
  | BGE
  | BLT
  | BGT
  | BLE
  | B
  | BL
  (* Special opcodes *)
  | LSL
  | ANDEQ
  | PUSH
  | POP
and offset = int
and reg = Reg of int
and shift =
  | ShiftImm of int
  (* | ShiftReg of reg           (\* TODO support this *\) *)
and rotate = int
and operand2 =
  | Operand2Imm of rotate * int    (* rotate * imm *)
  (* | Operand2Reg of shift * reg  TODO *)
and access =
  | InMem of int
  | InReg of reg
  | InImm of int
  | InLabel of string

let rFP = Reg 13                 (* frame pointer FP *)
let rLR = Reg 14                 (* link register LR *)
let rPC = Reg 15

type level = int
type allocater = access -> string

let string_of_opcode code = match code with
  | ADD -> "add"
  | SUB -> "sub"
  | RSB -> "rsb"
  | AND -> "and"
  | EOR -> "eor"
  | ORR -> "orr"
  | TST -> "tst"
  | TEQ -> "teq"
  | CMP -> "cmp"
  | LDR -> "ldr"
  | STR -> "str"
  | MUL -> "mul"
  | MLA -> "mla"
  | BEQ -> "beq"
  | BNE -> "bne"
  | BGE -> "bge"
  | BLT -> "blt"
  | BGT -> "bgt"
  | BLE -> "ble"
  | B -> "b"
  | BL -> "bl"
  | LSL -> "lsl"
  | ANDEQ -> "andeq"
  | PUSH -> "push"
  | POP -> "pop"
let string_of_reg reg = match reg with
  | Reg 13 -> "RSP"           (* Stack pointer *)
  | Reg 14 -> "LR"            (* Link register *)
  | Reg 15 -> "PC"            (* Program counter *)
  | Reg 16 -> assert false
  | Reg r -> (if (r < 0 || r > 16) then
                raise (Invalid_argument "Not a valid register number")
              else "r" ^ (string_of_int r))
let string_of_shift (s: shift) = match s with
  | ShiftImm i -> "#" ^ (string_of_int i)
let string_of_operand2 (op2: operand2) = match op2 with
  | Operand2Imm (rotate, imm) -> "#" ^ (string_of_int imm)
let string_of_access (access: access) = match access with
  | InMem i -> "[" ^ (string_of_int i) ^ "]"
  | InReg r -> string_of_reg r
  | InImm i -> "#" ^ (string_of_int i)
  | InLabel l -> "=" ^ l
  | _ -> assert false

let string_of_instr
    (inst: inst): string = match inst with
  | Inst_oper {oper_dst=dst;
               oper_op=op; _} when (op = PUSH || op = POP) -> (((string_of_opcode op) ^ " {" ^
                                                              (String.concat ", " (List.map string_of_access dst))
                                                              ^ "}" ))
  | Inst_oper {oper_src=src::_;
               oper_dst=dst::_;
               oper_op=op} -> ( ((string_of_opcode op) ^ " "
                                 ^ (string_of_access dst) ^ ", "
                                 ^ (string_of_access src)))
  | Inst_label label -> (label ^ ": ")
  | Inst_mov {mov_src=src; mov_dst=dst}-> ("mov " ^ (string_of_access dst) ^ ", " ^ (string_of_access src))
  | Inst_jump label -> ("bl " ^ label)
  | _ -> assert false
