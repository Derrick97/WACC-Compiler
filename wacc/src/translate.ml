module S = Symbol;;
module A = Ast;;
module F = Arm;;

type frag = string list
type size = int

type stmt = Arm.inst'

and access =
  | InFrame of int * size
  | InReg of Temp.temp * size
  | InLabel of string

and exp =
  | Imm of int * size
  | InAccess of access
  | Label of string

type frame = {
  mutable frame_counter: int;
  mutable frame_offset: int;
  mutable frame_locals: access array;
}

let new_label (): string = "l"
let new_namedlabel name = name

let strings: (string * string) list ref = ref []

let ex_temp (exp) = match exp with
  | InAccess (InReg (t, size)) -> t
  | _ -> assert false

let operand_of_exp (exp: exp): Arm.operand = match exp with
  | InAccess (InReg (t, sz)) -> Arm.OperReg t
  | Imm (i, sz) -> Arm.OperImm (i)
  | _ -> assert false  (* FIXME *)

let new_frame frame = {
  frame_counter = 0;
  frame_offset = 0;
  frame_locals = [| |];
}

let trans_call (fname: string)
    (args: exp list): (stmt list * exp) = begin
  let fname_label = new_namedlabel fname in
  let ilist = ref [] in
  let emit x = ilist := !ilist @ [x] in
  assert (List.length args <= 1); (* FIXME we only handle one argument for now *)
  List.iter (fun inst ->
      let t = F.new_temp() in
      match inst with
      | Imm (i, size) -> assert false
      | InAccess (InReg (t, sz)) -> begin
          emit(Arm.MOV ((List.nth F.caller_saved_regs 0), Arm.OperReg t), None);
        end
      | InAccess (InLabel label) -> begin
          emit(Arm.LDR (t, Arm.AddrLabel label), None);
          emit(Arm.MOV ((List.nth F.caller_saved_regs 0), Arm.OperReg t), None);
        end
      | _ -> assert false
    ) args;
  !ilist @ [(Arm.BL(fname_label), None)], InAccess(InReg (Arm.reg_RV, 4))
end

let trans_unop  (op: A.unop) (exp: exp): (stmt list * exp) = match op with
  | A.NotOp -> begin
      trans_call "wacc_len" [exp]
    end
  | A.NegOp -> begin
      let exp' = ex_temp exp in
      let t = F.new_temp () in
      ([(Arm.MOV(t, Arm.OperImm 0), None);
       (Arm.SUB(exp', t, (Arm.OperReg exp')), None)], exp)
    end
  | A.LenOp -> trans_call "wacc_len" [exp]
  | A.OrdOp -> trans_call "wacc_ord" [exp]
  | A.ChrOp -> trans_call "wacc_chr" [exp]

let trans_binop  (op: A.binop) (lhs: exp) (rhs: exp): (stmt list * exp) =
  let open Arm in
  let may_load = function
    | Imm (i, sz) -> begin
        let t = F.new_temp() in
        [(MOV(t, OperImm i), None)], t
      end
    | InAccess (InReg (t, _)) -> [], t
    | _ -> assert false in
  let lhsi, lhs' = may_load lhs in
  let oper = operand_of_exp rhs in
  let insts, v = (match op with
  | A.PlusOp -> ([ADD(lhs', lhs', oper), None], InAccess(InReg(lhs',1)))
  | A.MinusOp -> ([SUB(lhs', lhs', oper), None],InAccess(InReg(lhs',1)))
  | A.TimesOp -> begin
      let rhsi, rhs' = may_load rhs in
      (rhsi @ [MUL(lhs', lhs', rhs'), None],InAccess(InReg(lhs',1)))
    end
  | A.DivideOp -> failwith "TODO div"
  | A.AndOp -> ([AND (lhs', lhs', oper), None], InAccess(InReg(lhs',1)))
  | A.OrOp  -> ([ORR (lhs', lhs', oper), None], InAccess(InReg(lhs',1)))
  | A.ModOp -> (trans_call "wacc_mod" [lhs; rhs])
  | A.GeOp -> begin             (* FIXME we can use table driven methods here *)
      let t = F.new_temp () in
      ([(CMP(lhs', oper), None);
        (MOV(t, OperImm 1), Some GE);
         MOV(t, OperImm 0), Some LT], InAccess(InReg(t,1)))
    end
  | A.GtOp -> begin
      let t = F.new_temp () in
      ([CMP (lhs', oper), None;
        MOV(t, OperImm 1), Some GT;
        MOV(t, OperImm 0), Some LE ], InAccess(InReg(t,1)))
    end
  | A.LeOp -> begin
      let t = F.new_temp () in
      ([CMP (lhs', oper), None;
        MOV(t, OperImm 1), Some LE;
        MOV(t, OperImm 0), Some GT], InAccess(InReg(t,1)))
      end
  | A.LtOp -> begin
      let t = F.new_temp () in
      ([(CMP(lhs', oper), None);
        (MOV(t, OperImm 1), Some LT);
        (MOV(t, OperImm 0), Some GE)], InAccess(InReg(t,1)))
      end
  | A.EqOp -> begin
      let t = F.new_temp () in
      ([CMP (lhs', oper), None;
        MOV(t, OperImm 1), Some EQ;
        MOV(t, OperImm 0), Some NE], InAccess(InReg(t,1)))
      end
  | A.NeOp -> begin
      let t = F.new_temp () in
      ([(CMP (lhs', oper), None);
        (MOV(t, OperImm 1), Some NE);
        (MOV(t, OperImm 0), Some EQ)], InAccess(InReg(t,1)))
    end)
  in lhsi @ insts, v

let trans_lit    (l: A.literal): exp = match l with
  | A.LitInt i -> Imm (i, 4)
  | A.LitString s -> begin
      let label = "L" ^ new_namedlabel(s) in
      strings := (label, s)::!strings;
      InAccess (InLabel label)
    end
  | _ -> assert false

let trans_ifelse (cond: exp) (t: stmt list) (f: stmt list) = begin
  let true_l = new_namedlabel "if_then" in
  let false_l = new_namedlabel "if_else" in
  let end_l = new_namedlabel "if_end" in
  let cond_t = ex_temp cond in
  [Arm.CMP(cond_t, Arm.OperImm 1), None;
   Arm.LABEL(true_l),  Some Arm.EQ;] @ t                    @
                                       [Arm.B(end_l), None] @
  [Arm.LABEL(false_l), Some Arm.NE;] @ f @
  [Arm.LABEL(end_l), None]
end

let trans_var    (var: access): (stmt list * exp) = match var with
  | InFrame (offset, sz) ->
    let t = F.new_temp () in
    [Arm.LDR(t, Arm.AddrIndirect (Arm.reg_SP, offset)), None],
    InAccess(InReg(t, sz))
  | _ -> assert false

let trans_assign (lv: access) (rv: exp): (stmt list * exp) = begin
  let InFrame (offset, sz) = lv in
  match rv with
  | InAccess(InReg(t, sz)) ->
  ([Arm.STR(t, Arm.AddrIndirect (Arm.reg_SP, offset)), None],
   InAccess(InReg(t, sz)))
  | Imm (i, sz) -> begin
      let t = F.new_temp() in
      ([Arm.MOV(t, OperImm(i)), None;
        Arm.STR(t, Arm.AddrIndirect (Arm.reg_SP, offset)), None;
       ],
      InAccess(InReg(t, sz)))
    end
end

let trans_array  (var: access) (indices: exp list)
  = failwith "TODO trans_array"

let trans_while  (cond: exp) (body: stmt list) = begin
  let while_cond_l = new_namedlabel "while_cond" in
  let while_end_l =  new_namedlabel "while_done" in
  let cond_t = ex_temp cond in
  [Arm.CMP(cond_t, Arm.OperImm 1), None;
   Arm.B(while_cond_l), Some Arm.EQ;
   Arm.B(while_end_l), None]
end

let allocate_local (frame: frame) (size: size) =
  let offset = frame.frame_offset in
  let a = InFrame (offset, size) in
  frame.frame_offset <- offset + size;
  frame.frame_locals <- Array.append frame.frame_locals [|a|];
  a

let trans_noop: stmt list = []

let access_of_exp (exp: exp): access = failwith "TODO access of exp"

let function_prologue (frame: frame) (args: access list): unit = failwith "TODO prologue"
let function_epilogue (frame: frame) = failwith "TODO epilogue"

let print_insts (frame: frame) (insts: stmt list) =
  Printf.fprintf stdout ".data\n";
  List.iter (fun (l, s) ->
     (* print_int (String.length s); *)
     print_string (Printf.sprintf "%s:\n\t.ascii \"%s\"\n" l s);
    ) !strings;
  Printf.fprintf stdout ".text\n";
  Printf.fprintf stdout ".global main\n";
  Printf.fprintf stdout "main:\n";
  Printf.fprintf stdout "push {lr}\n";
  let local_size: int = (Array.fold_left (+) 0 (Array.map (fun x -> match x with
      | InFrame (t, sz) -> sz
      | _ -> assert false) frame.frame_locals)) in
  print_string ("sub sp, sp, #" ^ (string_of_int local_size) ^ "\n");
  List.iter (fun x ->
      print_endline( Arm.string_of_inst' x)) insts;
  print_string ("add sp, sp, #" ^ (string_of_int local_size) ^ "\n") ;
  Printf.fprintf stdout "pop {pc}\n"
