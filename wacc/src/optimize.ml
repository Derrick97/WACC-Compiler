
open Il ;;

let diff_operand op1 op2 =
  match (op1, op2) with
  | (OperReg(temp1), OperReg(temp2)) -> if (String.compare temp1 temp2 == 0) then true else false
  | (OperImm(imm1), OperImm(imm2)) -> imm1 == imm2
  | _ -> false

let rec peephole_optimize (insts: Il.il list) =
  let open Il in
  match insts with
  | [] -> []
  | [last] -> [last]
  | fst::snd::others -> begin
    match fst with
    | MOV (temp, op) -> begin
      match snd with
      | ADD (temp2, op2, op3) ->
       if diff_operand (OperReg(temp)) op3 then
          ADD (temp2, op2, op) :: peephole_optimize others
       else fst::peephole_optimize (snd::others)
      | SUB (temp2, op2, op3) ->
        if diff_operand (OperReg(temp)) op3 then
          SUB (temp2, op2, op) :: peephole_optimize others
        else fst::peephole_optimize (snd::others)
      | DIV (temp2, op2, op3) ->
        if diff_operand (OperReg(temp)) op3 then
          DIV (temp2, op2, op) :: peephole_optimize others
        else fst::peephole_optimize (snd::others)
      | MUL (temp2, op2, op3) ->
        if diff_operand (OperReg(temp)) op3 then
          MUL (temp2, op2, op) :: peephole_optimize others
        else fst::peephole_optimize (snd::others)
      | MOV (temp', op') ->
        if diff_operand (OperReg(temp)) op' && diff_operand (OperReg(temp')) op 
        then fst::peephole_optimize others
        else fst::peephole_optimize (snd::others)
      | _ -> fst::peephole_optimize (snd::others)
      end

    | _ -> fst::peephole_optimize (snd::others)
  end

  let inst_list1 = [PUSH ["r1"]; MOV ("r0", (OperImm(5))); MUL ("r5", (OperReg("r5")), (OperReg("r0"))); POP ["r1"]]

  let optm_inst = peephole_optimize inst_list1

  let rec print_insts inst_list =
  match inst_list with
  | [] -> print_string ""; print_newline ()
  | fst::others -> inst_to_print fst; print_newline (); print_insts others

  (*let () = print_insts optm_inst*)
