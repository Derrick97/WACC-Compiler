(* Tiling *)
open Tree;;
module A = Arm;;

let emit (inst: Arm.inst) =
  print_endline (A.string_of_inst inst)

let rec codegen_exp  (exp: Tree.exp): Temp.temp = match exp with
  | Temp (t, sz)  -> t
  | Const (i, sz) ->
    let t = A.new_temp () in
    emit(A.MOV(t, A.OperImm i)); t
  | Binop (PLUS, lhs, rhs) -> begin
      let lhse = codegen_exp lhs in
      let rhse = codegen_exp rhs in
      emit(A.ADD(lhse, rhse, A.OperReg rhse)); lhse
    end
  | Binop (MINUS, lhs, rhs) -> failwith "TODO"
  | _ -> assert false
and     codegen_stmt (stmt: Tree.stmt): unit = match stmt with
  | Move (lv, rv) -> ()
  | Exp exp -> ()
  | Jump (exp,exp') -> ()
  | CJump (relop, exp, exp', label) -> ()
  | Seq (stmt, stmtlist) -> ()
  | Label label -> ()
  | NoOp -> ()
