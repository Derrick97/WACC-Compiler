module IL = Il;;
module A = Ast_v2;;
module E = Env;;
module S = Semantic;;
module F = Arm;;
module RA = Register_allocator;;
open Env;;

type frag = string list
type size = int

type temp = string

type access = Env.access
type ctx = Env.env

type frame = {
  mutable frame_counter: int;
  mutable frame_offset: int;
  mutable frame_locals: access array;
}

let counter = ref 0
let strings = ref []
let rec zip a b = match (a, b) with
  | ([], _) -> []
  | (_, []) -> []
  | (x::xs, y::ys) -> (x, y)::(zip xs ys)

let new_label ?(prefix="L") (): string =
  let i = !counter in
  counter := !counter + 1;
  prefix ^ (string_of_int i)

let new_namedlabel name = name

let new_frame frame = {
  frame_counter = 0;
  frame_offset = 0;
  frame_locals = [| |];
}


let allocate_local (frame: frame) (size: size) =
  let offset = frame.frame_offset in
  let a = InFrame (offset, size) in
  frame.frame_offset <- offset + size;
  frame.frame_locals <- Array.append frame.frame_locals [|a|];
  a

let allocate_temp () = begin
  let c = !counter in
  counter := c+1;
  "t" ^ (string_of_int c)
end

let size_of_type = function
  | A.CharTy | A.BoolTy -> 1
  | A.IntTy -> 4
  | A.StringTy | A.PairTy _ | A.NullTy | A.PairTyy | A.ArrayTy _ -> 4

let rec trans_exp ctx exp =
  let open Il in
  let dst = allocate_temp() in
  let oper_reg r = OperReg r in
  let oper_imm i = OperImm i in
  begin match exp with
  | A.IdentExp       ident -> failwith "TODO"
  | A.ArrayIndexExp (ident, exps) -> failwith "TODO"
  | A.LiteralExp    (lit) -> begin
      let open Il in
      let insts = (match lit with
      | A.LitInt i -> [Il.LOAD (WORD, dst, (ADDR_LABEL (string_of_int i)))]
      | A.LitString s -> begin
          let label = new_label() in
          strings := (label, s)::!strings;
          [LOAD (WORD, dst, (ADDR_LABEL label))]
        end
      | A.LitBool b -> if b then
          [MOV (dst, (OperImm 1))]
        else
          [MOV (dst, (OperImm 0))]
      | A.LitChar c -> [MOV (dst, (OperImm (Char.code c)))]
      |  _ -> assert false) in
      dst, insts
    end
  | A.BinOpExp      ((lhs, _), binop, (rhs, _)) -> begin
      let (l, lhsi) = (trans_exp ctx lhs) in
      let (r, rhsi) = (trans_exp ctx rhs) in
      let (opl, opr) = (oper_reg l), (oper_reg r) in
      let  dst = allocate_temp() in
      let inst = (match binop with
       | A.PlusOp   -> [ADD (dst, opl, opr)]
       | A.MinusOp  -> [SUB (dst, opl, opr)]
       | A.TimesOp  -> [MUL (dst, opl, opr)]
       | A.DivideOp -> [DIV (dst, opl, opr)]
       | A.AndOp ->    [AND (dst, opl, opr)]
       | A.OrOp  ->    [ORR (dst, opl, opr)]
       | A.ModOp -> failwith "mod handled in frontend"
       | _ -> failwith "TODO")
      in
      dst, (lhsi @ rhsi @ inst)
    end
  | A.UnOpExp       (unop, (exp, _))  -> begin
      let (t, expi) = trans_exp ctx exp in
      let opt = oper_reg t in
      let dst = allocate_temp() in
      let insts = (match unop with
       | A.NotOp -> failwith "TODO"
       | A.NegOp -> [SUB(dst, OperImm 0, opt)]
       | A.LenOp | A.OrdOp | A.ChrOp -> failwith "should be desugared" ) in
      (dst, insts)
    end
  | A.CallExp       (fname, args) -> begin
      let argsi = List.map (fun (arge, _) -> trans_exp ctx arge) args in
      let insts = List.concat (List.map snd argsi) in
      let resultt = allocate_temp() in
      let argtemps = List.map fst argsi in
      let calli = trans_call ctx fname argtemps in
      resultt, (insts @ calli)
    end
  | A.NewPairExp    (lval, exp) -> failwith "TODO"
  | A.FstExp         exp  -> failwith "TODO"
  | A.SndExp         exp  -> failwith "TODO"
  | A.NullExp -> failwith "TODO"
  end

and trans_call (ctx: ctx)
    (fname: string)
    (args: temp list): (Il.il list) = begin
  let fname_label = new_namedlabel fname in
  let ilist = ref [] in
  let emit x = ilist := !ilist @ [x] in
  let split_reg_stack_passed regs =
    let reg_passed = ref [] in
    let stack_passed = ref [] in
    List.iteri (fun i x -> if i <= 3
                 then reg_passed := !reg_passed @ [x]
                 else stack_passed := !stack_passed @ [x]) regs;
    !reg_passed, !stack_passed
  in
  let reg_passed, stack_passed = split_reg_stack_passed args in
  (* First we mov the first 4 registers *)
  emit(Il.PUSH Arm.caller_saved_regs);
  List.iter (fun (d, s) -> emit(Il.MOV(d, Il.OperReg s))) (zip F.caller_saved_regs reg_passed);
  (* The other registers are pushed to stack *)
  List.iter (fun x -> emit (Il.PUSH [x])) (List.rev stack_passed);
  !ilist @ [Il.JUMP fname_label] @
  [(Il.POP F.caller_saved_regs)] @
  (List.map (fun x -> (Il.POP [x])) (stack_passed));
end


and trans_stmt ctx frame stmt = begin
  match stmt with
  | A.PrintStmt (newline, (exp, _)) -> begin
      let (v, insts) = trans_exp ctx exp in
      let callinsts = trans_call ctx "wacc_print" [v] in (* TODO add type for print *)
      insts @ callinsts
    end
  | _ -> []
end

and trans_prog ctx (decs, stmt) = begin
  let frame = new_frame() in
  let (stmt, _) = stmt in
  let insts = trans_stmt ctx frame stmt in
  List.iter (fun i -> print_endline (Il.show_il i)) insts;
  (* build CFG *)
  let liveout = Liveness.build insts in
  let igraph = Liveness.build_interference insts liveout in
  Liveness.show_interference igraph;
  let colormap = RA.allocate insts igraph in
  print_endline "Allocation";
  Hashtbl.iter (fun k v -> begin
        print_endline (Printf.sprintf "%s: %s" k v)
  end) colormap;
  (* List.iter (fun i -> print_endline (Il.show_il i)) insts; *)
  insts
end
