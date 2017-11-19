open Char
type label = string
type operand =
  | OperReg of reg * (shift option)
  | OperImm of int
  | OperChar of char
and inst =
  | ADD  of  reg * reg * operand
  | SUB  of  reg * reg * operand
  | AND  of  reg * reg * operand
  | ORR  of  reg * reg * operand
  | MUL  of  reg * reg * reg
  | SMULL of reg * reg * reg * reg
  | MOV  of  reg * operand
  | CMP  of  reg * operand
  | POP  of  reg list
  | PUSH of  reg list
  | LDR  of  reg * addr
  | LDRB of  reg * addr
  | STR  of  reg * addr
  | STRB of  reg * addr
  | EOR  of  reg * reg * operand
  | BL   of  label
  | LABEL of label
  | B of label
  | LTORG (*This instruction makes sure that the literal pools are assembled within the range of LDR instruction*)
          (*http://infocenter.arm.com/help/index.jsp?topic=/com.arm.doc.dui0041c/Babbfdih.html*)
and inst' = inst * cond option
and cond = GT | GE | LT | LE | EQ | NE | VS
and reg  = Temp.temp
and addr =
  | AddrLabel of string
  | AddrIndirect of reg * int
and shift =
  | ASR of int
  | LSL of int
  | LSR of int
  | ROR of int
  (* TODO support other shifts *)

let caller_saved_regs = [0;1;2;3]
let callee_saved_regs = [
  4;5;6;7;8;9;10;11;12
]

let reg_SP = 13
let reg_LR = 14
let reg_PC = 15
let reg_RV = 0                  (* R0 *)

let counter = ref 4

let new_temp () =
  let i = !counter in
  counter := i + 1; i

let rec string_of_reg = function
  | 13 -> "sp"
  | 14 -> "lr"
  | 15 -> "pc"
  | i when (i >= 0 && i < 13) -> "r" ^ (string_of_int i)
  | i -> "$r" ^ (string_of_int i)

and string_of_addr = function
  | AddrLabel label -> "=" ^ label
  | AddrIndirect (base, offset) -> begin
      if offset = 0 then
        "[" ^ (string_of_reg base) ^ "]"
      else
        "[" ^ (string_of_reg base) ^ ", #" ^ (string_of_int offset) ^ "]"
    end

and string_of_shift sh = match sh with
  | ASR i -> "asr #" ^ (string_of_int i)
  | LSL i -> "lsl #" ^ (string_of_int i)
  | LSR i -> "lsr #" ^ (string_of_int i)
  | ROR i -> "ror #" ^ (string_of_int i)

and string_of_operand (op:operand) = match op with
  | OperImm i -> "#" ^ (string_of_int i)
  | OperReg (r, shift) -> begin
      string_of_reg r ^ (
      match shift with
      | None -> ""
      | Some sh -> ", " ^ (string_of_shift sh))
    end
  | OperChar c -> begin
      let c = match c with
        | '\000' -> "\0"
        | _ -> String.make 1 c in
      "#'" ^ c ^ "'"
    end

and string_of_opcode = function
  | ADD _ -> "\tadds"
  | SUB _ -> "\tsubs"
  | MOV _ -> "\tmov"
  | POP _ -> "\tpop"
  | EOR _ -> "\teor"
  | PUSH _ -> "\tpush"
  | LDR _ -> "\tldr"
  | LDRB _ -> "\tldrb"
  | STRB _ -> "\tstrb"
  | STR _ -> "\tstr"
  | BL _ -> "\tbl"
  | AND _ -> "\tand"
  | ORR _ -> "\torr"
  | MUL _ -> "\tmul"
  | CMP _ -> "\tcmp"
  | LABEL _ -> ""               (* Not used *)
  | B _ -> "\tb"
  | SMULL _ -> "\tsmull"

let string_of_inst (inst: inst) =
  let opcode_str = string_of_opcode inst in
  match inst with
  | ADD (dst, op1, op2)
  | EOR (dst, op1, op2)
    | SUB (dst, op1, op2)
    | AND (dst, op1, op2)
    | ORR (dst, op1, op2) -> (opcode_str ^ " " ^ (string_of_reg dst) ^ ", " ^
                            (string_of_reg op1) ^ ", " ^
                            (string_of_operand op2))
  | POP ops | PUSH ops -> (opcode_str) ^
                              " {" ^
                              (String.concat ", " (List.map (string_of_reg) ops)) ^
                              "}"
  | MOV (op1, op2) -> "\tmov " ^ (string_of_reg op1) ^ " " ^ (string_of_operand op2)
  | LDR (op1, op2) | STR (op1, op2) | STRB (op1, op2) | LDRB (op1, op2) -> opcode_str ^ " "
                                                        ^ (string_of_reg op1) ^
                                                        ", " ^
                                                        (string_of_addr op2)
  | BL s -> opcode_str ^ " " ^ s
  | MUL  (r0,r1,r2) -> "\tmul " ^ (String.concat ", " (List.map (string_of_reg) [r0;r1;r2]))
  | CMP  (reg, op) -> "\tcmp " ^ (string_of_reg reg) ^ " " ^ (string_of_operand op)
  | LABEL label -> label ^ ":"
  | B label -> "\tb " ^ label
  | SMULL (r0, r1, r2, r3) -> "\tsmull " ^ (String.concat ", " (List.map string_of_reg [r0;r1;r2;r3]))

let string_of_cond = function
  | GT -> "gt"
  | GE -> "ge"
  | LT -> "lt"
  | LE -> "le"
  | EQ -> "eq"
  | NE -> "ne"
  | VS -> "vs"


let string_of_inst' (inst: inst') =
  let inst, cond = inst in
  let opcode_str = string_of_opcode inst in
  let opcode_str = match cond with
    | None -> opcode_str
    | Some c -> opcode_str ^ (string_of_cond c) in
  match inst with
  | ADD (dst, op1, op2)
  | EOR (dst, op1, op2)
  | SUB (dst, op1, op2)
  | AND (dst, op1, op2)
  | ORR (dst, op1, op2) -> (opcode_str ^ " " ^ (string_of_reg dst) ^ ", " ^
                           (string_of_reg op1) ^ ", " ^
                           (string_of_operand op2))
  | PUSH ops | POP ops -> opcode_str ^
                              " {" ^
                              (String.concat ", " (List.map (string_of_reg) ops)) ^
                              "}"
  | MOV (op1, op2) -> opcode_str ^ " " ^ (string_of_reg op1) ^ ", " ^ (string_of_operand op2)
  | LDR (op1, op2) | STR (op1, op2) | STRB (op1, op2)
  | LDRB (op1, op2) -> opcode_str ^ " "
                       ^ (string_of_reg op1) ^
                       ", " ^ (string_of_addr op2)
  | BL s -> opcode_str ^ " " ^ s
  | MUL  (r0,r1,r2) ->  opcode_str ^ " " ^ (String.concat ", " (List.map (string_of_reg) [r0;r1;r2]))
  | CMP  (reg, op) ->  opcode_str ^ " " ^ (string_of_reg reg) ^ ", " ^ (string_of_operand op)
  | LABEL label -> label ^ ":"
  | B label -> opcode_str ^ " " ^ label
  | SMULL (r0, r1, r2, r3) -> opcode_str ^ " " ^ (String.concat ", " (List.map string_of_reg [r0;r1;r2;r3]))

let add ?cond dst reg op = (ADD (dst,reg,op), cond)
let push ?cond ops = (PUSH (ops), cond)
let pop ?cond ops = (POP (ops), cond)
let sub ?cond dst reg op = (SUB (dst,reg,op), cond)
let eor ?cond dst reg op = (EOR (dst,reg,op), cond)
let annd ?cond dst reg op = (AND (dst,reg,op), cond)
let orr ?cond dst reg op = (ORR (dst,reg,op), cond)
let mov ?cond op1 op2 = (MOV (op1,op2), cond)
let load ?cond reg1 addr = (LDR (reg1, addr), cond)
let loadb ?cond reg1 addr = (LDRB (reg1, addr), cond)
let str ?cond reg1 addr = (STR (reg1, addr), cond)
let strb ?cond reg1 addr = (STRB (reg1, addr), cond)
let jump ?cond label = (B (label), cond)
let bl ?cond label = (BL (label), cond)
let cmp ?cond reg op = (CMP (reg, op), cond)
let labels label = (LABEL (label), None)
let mul ?cond reg1 reg2 reg3 = (MUL (reg1, reg2, reg3), cond)
let smull ?cond rdlo rdhi rn rm = (SMULL (rdlo, rdhi, rn, rm), cond)
