type frame
type access
type reg
type inst
type opcode
type size

val new_frame: string -> bool list -> frame
val allocate_local: frame -> size -> access

val regSP: reg
val regPC: reg
val regLR: reg
val registers: reg list

val argument_regs: reg list
val caller_saves: reg list
val callee_saves: reg list

val push: access list -> inst
val pop: access list -> inst
val mov: access -> access -> inst

val store: access -> access -> inst
val load: access -> access -> inst

val br: inst
val add: access -> access -> access -> inst
val sub: access -> access -> access -> inst
