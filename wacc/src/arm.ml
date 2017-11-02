type label = string
type operand =
  | OperReg of reg
  | OperImm of int
and inst =
  | ADD  of  reg * reg * operand
  | SUB  of  reg * reg * operand
  | AND  of  reg * reg * operand
  | ORR  of  reg * reg * operand
  | MUL  of  reg * reg * reg
  | MOV  of  reg * operand
  | CMP  of  reg * operand
  | POP  of  reg list
  | PUSH of  reg list
  | LDR  of  reg * addr
  | STR  of  reg * addr
  | BL   of  label
  | B of label
and inst' = inst * cond option
and cond = GT | GE | LT | LE | EQ | NE
and reg  = Temp.temp
and addr =
  | AddrLabel of string
  | AddrIndirect of reg * int

let caller_saved_regs = [0; 1; 2; 3];;
let callee_saved_regs = [
  4;5;6;7;8;9;10;11;12
];;

let reg_SP = 13
let reg_LR = 14
let reg_PC = 15
let reg_RV = 0

let new_temp () = 0

let string_of_reg = function
  | 13 -> "sp"
  | 14 -> "lr"
  | 15 -> "pc"
  | i when (i >= 0 && i < 13) -> "r" ^ (string_of_int i)
  | _ -> assert false

let string_of_addr = function
  | AddrLabel label -> "=" ^ label
  | AddrIndirect (base, offset) -> begin
      if offset = 0 then
        "[" ^ (string_of_reg base) ^ "]"
      else
        "[" ^ (string_of_reg base) ^ ", #" ^ (string_of_int offset) ^ "]"
    end

and string_of_operand (op:operand) = failwith "TODO"
and string_of_opcode = function
  | ADD _ -> "add"
  | SUB _ -> "sub"
  | MOV _ -> "mov"
  | POP _ -> "pop"
  | PUSH _ -> "push"
  | LDR _ -> "ldr"
  | STR _ -> "str"
  | BL _ -> "bl"
  | AND _ -> "and"
  | ORR _ -> "orr"

let string_of_inst (inst: inst) =
  let opcode_str = string_of_opcode inst in
  match inst with
  | ADD (dst, op1, op2) | SUB (dst, op1, op2) |
    AND (dst, op1, op2) | ORR (dst, op1, op2) -> (opcode_str ^ " " ^ (string_of_reg dst) ^ ", " ^
                            (string_of_reg op1) ^ ", " ^
                            (string_of_operand op2))
  | (PUSH ops) | (POP ops) -> (opcode_str) ^
                              " {" ^
                              (String.concat ", " (List.map (string_of_reg) ops)) ^
                              "}"
  | MOV (op1, op2) -> "mov" ^ (string_of_reg op1) ^ " " ^ (string_of_operand op2)
  | LDR (op1, op2) | STR (op1, op2) -> opcode_str ^ " "
                                                        ^ (string_of_reg op1) ^
                                                        ", " ^
                                                        (string_of_addr op2)
  | BL s -> opcode_str ^ " " ^ s
