type label = string
and reg
and addr
and operand =
  | OperReg of reg
and frame
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

val reg_PC: reg
val reg_LR: reg
val reg_SP: reg
val reg_RV: reg

val caller_saved_regs: reg list
val caller_saved_regs: reg list

val string_of_inst: inst -> string
val string_of_reg: reg -> string
val string_of_addr: addr -> string
val print_frame: frame -> out_channel -> unit
val new_frame: string -> frame

val allocate_temp: frame -> access
val allocate_local: frame -> access

val load: access -> access -> inst

val store: access -> access -> inst
val load: access -> access -> inst
val mov: access -> access -> inst
val (<:): frame -> inst -> unit
