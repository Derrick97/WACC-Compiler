type label = string
type operand =
  | OperReg of reg
and access =
  | InMem of addr
  | InReg of reg
and inst =
  | ADD  of  reg * reg * operand
  | SUB  of  reg * reg * operand
  | AND  of  reg * reg * operand
  | ORR  of  reg * reg * operand
  | MOV  of  reg * operand
  | POP  of  reg list
  | PUSH of  reg list
  | LDR  of  reg * addr
  | STR  of  reg * addr
  | BL   of  label

and reg  = int
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


let print_frame _ = failwith "TODO"
let access_of_reg _ = failwith "TODO"
let access_of_addr _ = failwith "TODO"
let mov _ = failwith "TODO"
let load _ = failwith "TODO"
let store _ = failwith "TODO"
let allocate_local _ = failwith "TODO"
let allocate_temp _ = failwith "TODO"
let (<:) frame inst = failwith "TODO"



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

(* let allocate_local *)
(*     (frame: frame): access  = ( *)
(*   let r = AccessAddr (Addr (reg_SP, frame.offset)) in *)
(*   let size = 4 in *)
(*   frame.offset <- frame.offset + size; *)
(*   ignore(Array.append frame.locals [| r |]); *)
(*   r);; *)

(* let allocate_temp *)
(*     (frame: frame): access = *)
(*   begin *)
(*   assert (frame.counter < 13); *)
(*   let r = AccessReg (Reg frame.counter) in *)
(*   frame.counter <- frame.counter + 1; *)
(*   ignore(Array.append frame.temps [| r |]); r *)
(*   end *)

(* let (<:) (frame: frame) (inst): unit = ( *)
(*   ignore(frame.instructions <- Array.append frame.instructions [| inst |]); ()) *)

(* let mov (dst: access) (src: access): inst = *)
(*   MOV (dst, src) *)

(* let store (dst: access) (src: access) = *)
(*   STR (src, dst);; *)

(* let load (dst: access) (src: access) = *)
(*   LDR (src, dst);; *)

(* let access_of_lit _ = failwith "TODO" *)
(* let access_of_reg _ = failwith "TODO" *)

(* let print_frame (frame: frame) (out: out_channel): unit = *)
(*   let open Printf in *)
(*   fprintf out "%s\n" (frame.name ^ ":"); *)
(*   let stack_size = ((Array.length frame.locals)*4) in *)
(*   let push    = PUSH ([(AccessReg reg_PC)]) in *)
(*   let alloc   = SUB (AccessReg reg_SP, AccessReg reg_SP, AccessImm stack_size) in *)
(*   let dealloc = ADD (AccessReg reg_SP, AccessReg reg_SP, AccessImm stack_size) in *)
(*   let pop     = POP ([(AccessReg reg_PC)]) in *)
(*   let insts = [push;            (\* push the link register *\) *)
(*                alloc] @         (\* allocate stack locals *\) *)
(*               (Array.to_list frame.instructions) @ *)
(*               [dealloc;         (\* deallocate stack locals *\) *)
(*                pop]             (\* pop the link register, return *\) *)
(*   in *)
(*   List.iter (fun x -> fprintf out "\t%s\n" (string_of_inst x)) insts *)

(* let access_of_int i = AccessImm i *)
