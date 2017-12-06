
open Il ;;

let eq_operand temp op =
  match op with
  | OperReg(temp') -> (String.compare temp temp' == 0)
  | _ -> false

let rec peephole_optimize (insts: Il.il list) =
  let open Il in
  match insts with
  | [] -> []
  (*The 1st case: MOV a reg into itself*)
  | MOV (temp, op)::others when eq_operand temp op -> peephole_optimize others
  (*The 2nd case: MOV a value into a reg and then do arithemetic on that reg immediately *)
  | MOV (temp, op)::ADD (temp2, op2, op3)::others when eq_operand temp op3 ->
      ADD (temp2, op2, op) :: peephole_optimize others
  | MOV (temp, op)::SUB (temp2, op2, op3)::others when eq_operand temp op3 ->
      SUB (temp2, op2, op) :: peephole_optimize others
  | MOV (temp, op)::DIV (temp2, op2, op3)::others when eq_operand temp op3 ->
      DIV (temp2, op2, op) :: peephole_optimize others
  (*The 3nd case: MOV a b then MOV b a*)
  | MOV (temp, op)::MOV (temp', op')::others when eq_operand temp op' && eq_operand temp' op ->
      MOV (temp, op) :: peephole_optimize others
  | fst::others -> fst::peephole_optimize others




  let inst_list1 = [PUSH ["r1"]; MOV ("r0", (OperImm(5))); MUL ("r5", (OperReg("r5")), (OperReg("r0"))); POP ["r1"]]

  let optm_inst = peephole_optimize inst_list1

  let rec print_insts inst_list =
  match inst_list with
  | [] -> print_string ""; print_newline ()
  | fst::others ->inst_to_print fst; print_newline (); print_insts others

  (*let () = print_insts optm_inst*)
