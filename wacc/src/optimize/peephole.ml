open Il ;;
let eq_operand temp op =
  match op with
  | OperReg(temp') -> (String.compare temp temp' == 0)
  | _ -> false

let rec optimize (insts: Il.il list) =
  let open Il in
  match insts with
  | [] -> []
  (*The 1st case: MOV a reg into itself*)
  (* | MOV (temp, OperImm i)::others -> LOAD (WORD, temp, ADDR_LABEL (string_of_int i))::optimize others *)
  | MOV (temp, op)::others when eq_operand temp op -> optimize others
  (*The 2nd case: MOV a value into a reg and then do arithemetic on that reg immediately *)
  | ((MOV (temp, (OperImm i))))::ADD (temp2, op2, op3)::others when eq_operand temp op3 && i > 1024->
      ((MOV (temp, (OperImm i))))::ADD (temp2, op2, op3)::(optimize others)
  | MOV (temp, op)::ADD (temp2, op2, op3)::others when eq_operand temp op3 ->
      ADD (temp2, op2, op) :: optimize others
  | MOV (temp, op)::SUB (temp2, op2, op3)::others when eq_operand temp op3 ->
      SUB (temp2, op2, op) :: optimize others
  | MOV (temp, op)::DIV (temp2, op2, op3)::others when eq_operand temp op3 ->
      DIV (temp2, op2, op) :: optimize others
  (*The 3nd case: MOV a b then MOV b a*)
  | MOV (temp, op)::MOV (temp', op')::others when eq_operand temp op' && eq_operand temp' op ->
     MOV (temp, op) :: optimize others
  | (NOOP _::others) -> optimize others
  | fst::others -> fst::optimize others
