type access
type reg
type label = string
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

val reg_PC: reg
val reg_LR: reg
val reg_SP: reg

val string_of_reg: reg -> string
val string_of_access: access -> string
val string_of_inst: inst -> string

type frame
val new_frame: string -> frame
val print_frame: frame -> out_channel -> unit
val allocate_local: frame -> access
val allocate_temp: frame -> access

val (<:): frame -> inst -> unit

val access_of_lit: Ast.literal -> access
val access_of_reg: reg -> access
val access_of_int: int -> access

val mov: access -> access -> inst

val store: access -> access -> inst
val load: access -> access -> inst
