module Codegen_Arm = struct
  val setup unit -> unit
  val codegen_stmt = Tree.stmt -> Arm_Asm.inst
  val codegen_exp = Tree.exp -> Arm_Asm.inst
  (* TODO *)
end
