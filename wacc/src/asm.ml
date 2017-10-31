type access =
  | AccessImm of int
  | AccessAddr of addr
  | AccessReg of reg
  | AccessSym of string
and label = string
and inst =
  | ADD  of  access * access * access
  | SUB  of  access * access * access
  | AND  of  access * access * access
  | ORR  of  access * access * access
  | MOV  of  access * access
  | POP  of  access list
  | PUSH of  access list
  | LDR  of  access * access
  | STR  of  access * access
  | BL   of  label
and reg  = Reg  of int
and addr = Addr of reg * int    (* base register * offset *)
and lit =
  | Lit_char of char
  | Lit_int of int
  | Lit_addr of string

let string_of_reg = function
  | Reg 13 -> "sp"
  | Reg 14 -> "lr"
  | Reg 15 -> "pc"
  | Reg i when (i >= 0 && i < 13) -> "r" ^ (string_of_int i)
  | _ -> assert false


let reg_PC = Reg 13
let reg_LR = Reg 14
let reg_SP = Reg 15

let reg_RV = Reg 0

let caller_saved_regs = [Reg 0; Reg 1; Reg 2; Reg 3];;
let callee_saved_regs = [
  (* TODO 4 -  12 *)
];;

let string_of_access
    (operand: access) = match operand with
  | AccessImm  i -> "#" ^ (string_of_int i)
  | AccessReg r -> string_of_reg r
  | AccessSym s -> "=" ^ s
  | AccessAddr (Addr (base, offset)) -> begin
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
    AND (dst, op1, op2) | ORR (dst, op1, op2) -> (opcode_str ^ " " ^ (string_of_access dst) ^ ", " ^
                            (string_of_access op1) ^ ", " ^
                            (string_of_access op2))
  | (PUSH ops) | (POP ops) -> (opcode_str) ^
                              " {" ^
                              (String.concat ", " (List.map (string_of_access) ops)) ^
                              "}"
  | MOV (op1, op2) | LDR (op2, op1) | STR (op1, op2) -> opcode_str ^
                                                        " "
                                                        ^ (string_of_access op1) ^
                                                        ", " ^
                                                        (string_of_access op2)
  | BL s -> opcode_str ^ " " ^ s

(* Activation record (AR) *)
type frame = {
  mutable name: string;
  mutable counter: int;         (* used for generating locals *)
  mutable offset: int;
  mutable locals: access array;
  mutable temps: access array;
  mutable instructions: inst array;
  level: int;
}

let new_frame (name:string) =
  { name = name;
    counter=(4);
    offset=0;
    locals= [| |];
    temps=[| |];
    level=0;
    instructions=[| |]}

let allocate_local
    (frame: frame): access  = (
  let r = AccessAddr (Addr (reg_SP, frame.offset)) in
  let size = 4 in
  frame.offset <- frame.offset + size;
  ignore(Array.append frame.locals [| r |]);
  r);;

let allocate_temp
    (frame: frame): access =
  begin
  assert (frame.counter < 13);
  let r = AccessReg (Reg frame.counter) in
  frame.counter <- frame.counter + 1;
  ignore(Array.append frame.temps [| r |]); r
  end

let (<:) (frame: frame) (inst): unit = (
  ignore(frame.instructions <- Array.append frame.instructions [| inst |]); ())

let mov (dst: access) (src: access): inst =
  MOV (dst, src)

let store (dst: access) (src: access) =
  STR (src, dst);;

let load (dst: access) (src: access) =
  LDR (src, dst);;

let access_of_lit _ = failwith "TODO"
let access_of_reg _ = failwith "TODO"

let args_regs = [Reg 0; Reg 1; Reg 2; Reg 3];;

let print_frame (frame: frame) (out: out_channel): unit =
  let open Printf in
  fprintf out "%s\n" (frame.name ^ ":");
  let stack_size = ((Array.length frame.locals)*4) in
  let push    = PUSH ([(AccessReg reg_PC)]) in
  let alloc   = SUB (AccessReg reg_SP, AccessReg reg_SP, AccessImm stack_size) in
  let dealloc = ADD (AccessReg reg_SP, AccessReg reg_SP, AccessImm stack_size) in
  let pop     = POP ([(AccessReg reg_PC)]) in
  let insts = [push;            (* push the link register *)
               alloc] @         (* allocate stack locals *)
              (Array.to_list frame.instructions) @
              [dealloc;         (* deallocate stack locals *)
               pop]             (* pop the link register, return *)
  in
  List.iter (fun x -> fprintf out "\t%s\n" (string_of_inst x)) insts

let access_of_int i = AccessImm i
