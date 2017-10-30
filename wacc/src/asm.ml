type operand =
  | OperImm of int
  | OperAddr of addr
  | OperReg of reg
  | OperSym of string
and label = string
and inst =
  | ADD  of  operand * operand * operand
  | SUB  of  operand * operand * operand
  | AND  of  operand * operand * operand
  | ORR  of  operand * operand * operand
  | MOV  of  operand * operand
  | POP  of  operand list
  | PUSH of  operand list
  | LDR  of  operand * operand
  | STR  of  operand * operand
  | BL   of  label

and data_type =
  | BYTE
  | WORD
and reg  = Reg  of int
and addr = Addr of reg * int    (* base register * offset *)
and lit =
  | Lit_char of char
  | Lit_int of int
  | Lit_addr of string

let reg_PC = Reg 15
let reg_LR = Reg 14
let reg_SP = Reg 13

let string_of_reg = function
  | Reg 13 -> "sp"
  | Reg 14 -> "lr"
  | Reg 15 -> "pc"
  | Reg i when (i >= 0 && i < 13) -> "r" ^ (string_of_int i)
  | _ -> assert false

(* let string_of_access *)
(*     (access: access) = match access with *)
(*   | InReg r -> string_of_reg r *)
(*   | InLabel l -> "=" ^ l *)
(*   | InMem m -> "[?m" ^ (string_of_int m) ^ "]" *)
(*   | InImm i -> "#" ^ (string_of_int i) *)

let string_of_operand
    (operand: operand) = match operand with
  | OperImm  i -> "#" ^ (string_of_int i)
  | OperReg r -> string_of_reg r
  | OperSym s -> "=" ^ s
  | OperAddr (Addr (base, offset)) -> begin
      if offset = 0 then
        "[" ^ (string_of_reg base) ^ "]"
      else
        "[" ^ (string_of_reg base) ^ ", #" ^ (string_of_int offset) ^ "]"
    end

let string_of_opcode = function
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
    AND (dst, op1, op2) | ORR (dst, op1, op2) -> (opcode_str ^ " " ^ (string_of_operand dst) ^ ", " ^
                            (string_of_operand op1) ^ ", " ^
                            (string_of_operand op2))
  | (PUSH ops) | (POP ops) -> (opcode_str) ^
                              " {" ^
                              (String.concat ", " (List.map (string_of_operand) ops)) ^
                              "}"
  | MOV (op1, op2) | LDR (op2, op1) | STR (op1, op2) -> opcode_str ^
                                                        " "
                                                        ^ (string_of_operand op1) ^
                                                        ", " ^
                                                        (string_of_operand op2)
  | BL s -> opcode_str ^ " " ^ s
