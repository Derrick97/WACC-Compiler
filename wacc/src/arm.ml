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
  | LABEL of label
  | B of label
and inst' = inst * cond option
and cond = GT | GE | LT | LE | EQ | NE
and reg  = Temp.temp
and addr =
  | AddrLabel of string
  | AddrIndirect of reg * int

let caller_saved_regs = [0;1;2;3];;
let callee_saved_regs = [
  4;5;6;7;8;9;10;11;12
];;

let reg_SP = 13
let reg_LR = 14
let reg_PC = 15
let reg_RV = 0                  (* R0 *)

let counter = ref 4

let new_temp () =
  let i = !counter in
  counter := i + 1; i

let string_of_reg = function
  | 13 -> "sp"
  | 14 -> "lr"
  | 15 -> "pc"
  | i when (i >= 0 && i < 13) -> "r" ^ (string_of_int i)
  | i -> "$r" ^ (string_of_int i)

let string_of_addr = function
  | AddrLabel label -> "=" ^ label
  | AddrIndirect (base, offset) -> begin
      if offset = 0 then
        "[" ^ (string_of_reg base) ^ "]"
      else
        "[" ^ (string_of_reg base) ^ ", #" ^ (string_of_int offset) ^ "]"
    end

and string_of_operand (op:operand) = match op with
  | OperImm i -> "#" ^ (string_of_int i)
  | OperReg r -> string_of_reg (r)

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
  | MUL _ -> "mul"
  | CMP _ -> "cmp"
  | LABEL _ -> ""               (* Not used *)
  | B _ -> "b"

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
  | MOV (op1, op2) -> "mov " ^ (string_of_reg op1) ^ " " ^ (string_of_operand op2)
  | LDR (op1, op2) | STR (op1, op2) -> opcode_str ^ " "
                                                        ^ (string_of_reg op1) ^
                                                        ", " ^
                                                        (string_of_addr op2)
  | BL s -> opcode_str ^ " " ^ s
  | MUL  (r0,r1,r2) -> "mul " ^ (String.concat ", " (List.map (string_of_reg) [r0;r1;r2]))
  | CMP  (reg, op) -> "cmp " ^ (string_of_reg reg) ^ " " ^ (string_of_operand op)
  | LABEL label -> label ^ ":"
  | B label -> "b " ^ label

let string_of_cond = function
  | GT -> "gt"
  | GE -> "ge"
  | LT -> "lt"
  | LE -> "le"
  | EQ -> "eq"
  | NE -> "ne"


let string_of_inst' (inst: inst') =
  let inst, cond = inst in
  let opcode_str = string_of_opcode inst in
  let opcode_str = match cond with
    | None -> opcode_str
    | Some c -> opcode_str ^ (string_of_cond c) in
  match inst with
  | ADD (dst, op1, op2)
  | SUB (dst, op1, op2)
  | AND (dst, op1, op2)
  | ORR (dst, op1, op2) -> (opcode_str ^ " " ^ (string_of_reg dst) ^ ", " ^
                           (string_of_reg op1) ^ ", " ^
                           (string_of_operand op2))
  | (PUSH ops) | (POP ops) -> opcode_str ^
                              "{" ^
                              (String.concat ", " (List.map (string_of_reg) ops)) ^
                              "}"
  | MOV (op1, op2) -> opcode_str ^ " " ^ (string_of_reg op1) ^ ", " ^ (string_of_operand op2)
  | LDR (op1, op2) | STR (op1, op2) -> opcode_str ^ " "
                                       ^ (string_of_reg op1) ^
                                       ", " ^
                                       (string_of_addr op2)
  | BL s -> opcode_str ^ " " ^ s
  | MUL  (r0,r1,r2) ->  opcode_str ^ " " ^ (String.concat ", " (List.map (string_of_reg) [r0;r1;r2]))
  | CMP  (reg, op) ->  opcode_str ^ " " ^ (string_of_reg reg) ^ ", " ^ (string_of_operand op)
  | LABEL label -> label ^ ":"
  | B label -> opcode_str ^ " " ^ label
