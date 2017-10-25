module S = Symbol
open Ast

module Temp: sig
  type temp
  val newtemp: unit -> temp
  (* module Table *)
  type label = string
  val newlabel: unit -> label
  val namedlabel: string -> label
end = struct
  type label = string
  type temp =
    | Reg of int
    | Memory of int
  let newtemp () = Reg 1
  let newlabel () = "foo"
  let namedlabel name = name
end


module ArmInst = struct
  (* 4.1.2 might useful *)
  type inst =
    | InstDp of opcode * reg * operand2
    | InstMult of opcode
    | InstSdt
    | InstBr of opcode * string
    | InstSp of inst_sp
    | InstLabel of string
  and inst_sp =
    | Lsl of reg * reg
    | Halt
  and opcode =
    (* Data processing opcodes *)
    | Add
    | Sub
    | Rsb
    | And
    | Eor
    | Orr
    | Mov
    | Tst
    | Teq
    | Cmp
    (* Single Data Transfer opcodes *)
    | Ldr
    | Str
    (* Multiply opcodes *)
    | Mul
    | Mla
    (* Branching opcodes *)
    | Beq
    | Bne
    | Bge
    | Blt
    | Bgt
    | Ble
    | B
    (* Special opcodes *)
    | OpLsl
    | Andeq
  and operand2 =
    | Rm of reg
    | Imm of int
  and reg = Reg of int
  and shift =
    | ConstShift of int
    | RegShift of reg

  let string_of_opcode code = match code with
    | Add -> "add"
    | Sub -> "sub"
    | Rsb -> "rsb"
    | And -> "and"
    | Eor -> "eor"
    | Orr -> "orr"
    | Mov -> "mov"
    | Tst -> "tst"
    | Teq -> "teq"
    | Cmp -> "cmp"
    | Ldr -> "ldr"
    | Str -> "str"
    | Mul -> "mul"
    | Mla -> "mla"
    | Beq -> "beq"
    | Bne -> "bne"
    | Bge -> "bge"
    | Blt -> "blt"
    | Bgt -> "bgt"
    | Ble -> "ble"
    | B -> "b"
    | OpLsl -> "lsl"
    | Andeq -> "andeq"
  let string_of_reg reg = match reg with
    | Reg 13 -> "rsp"           (* Stack pointer *)
    | Reg 14 -> "rlr"           (* Link register *)
    | Reg 15 -> "pc"            (* Program counter *)
    | Reg 16 -> "aspr"
    | Reg r -> (if (r < 0 || r > 16) then
                  raise (Invalid_argument "Not a valid register number")
                else "r" ^ (string_of_int r))
  let string_of_shift s = match s with
    | _ -> "shift"
  let string_of_op2 op2 = match op2 with
    | Rm reg -> "Rm" ^ (string_of_reg reg)
    | Imm i -> "#" ^ (string_of_int i)
  let string_of_instr instr = match instr with
    | InstDp (op, reg, op2) -> (string_of_opcode op) ^ " " ^ (string_of_reg reg) ^ " " ^ (string_of_op2 op2)
    | InstLabel label -> label ^ ":"
    | InstBr (cond, label) -> (string_of_opcode cond) ^ " " ^ label
    | _ -> "TODO"
end


module Codegen: sig
  type 'a t
  val create: unit -> 'a t
  val add: 'a t -> 'a -> 'a t
end = struct
  type 'a t = 'a list
  let create () = []
  let add c inst = c @ [inst]
end

module Frame: sig
  (* We use a frame to organize codegen *)
  type frame
  type level
  type access

  val new_frame: level -> frame
  val formals: frame -> unit
  val outtermost: level
end = struct
  type level = int

  type access =
    | InMem of int
    | InReg of ArmInst.reg

  type frame = {
    mutable counter: int;
    mutable locals: access list;
    level: int;
  }

  (* Type representing ARM instructions *)
  type inst =
    | Label

  type operand =
    | OperandReg of ArmInst.reg
    | OperandImm of int

  let outtermost = 0
  let static_links = ref []

  let new_frame level = { counter=(-1); locals=[]; level=level}
  let formals frame = ()

  let allocate_local frame = (
    frame.counter <- frame.counter + 1;
    assert (frame.counter < 13);
    let r = ArmInst.Reg frame.counter in
    ignore(frame.locals = (InReg r)::frame.locals);
    r
  )

  type codegen_env = operand S.table

  let lookup_local table name = raise (Failure "TODO lookup_local")

  let trans_lit lit = match lit with
    | LitBool b -> (if b then OperandImm 1 else OperandImm 0 )
    | LitInt i -> OperandImm i
    | LitChar c -> OperandImm (Char.code c)
    | _ -> raise (Failure "TODO")

  (* translation follows the following rule:
     trans(e) = <c, p>
  *)
  let rec trans_exp (table: codegen_env) frame exp = let open ArmInst in (match exp with
    | BinOpExp (lhs, op, rhs, _) -> (
        let tr = trans_exp table frame in
        let (lhsc, lhsp) = tr lhs in  (*-- *c denotes commands, *p denotes pure expressions, see CMU fp's notes --*)
        let (rhsc, rhsp) = tr rhs in
        let o = (match op with
        | PlusOp -> Add
        | MinusOp -> Sub
        | TimesOp -> Mul
        | _ -> raise (Failure "TODO match op")) in
        let dst = allocate_local frame in
        let op2 = (match rhsp with
            | OperandReg r -> Rm r
            | OperandImm num -> Imm num
        ) in
        let inst = InstDp (o, dst, op2) in
        (lhsc @ rhsc @ [inst], OperandReg dst)
      )
    | IdentExp (ident, _) -> (
        let InReg r = lookup_local table ident in
        ([], OperandReg r)
      )
    | LiteralExp (lit, _) -> ([], trans_lit lit)
    | _ -> raise (Failure "TODO other expression"))

  let rec trans_stmt table frame stmt = match stmt with
    (* | SeqStmt (stmt::stmts) -> () *)
    | VarDeclStmt (ty,name,exp,_) -> (
        trans_decl table frame stmt
      )
    | _ -> raise (Failure "TODO trans_stmt")

  and trans_decl table frame decl = match decl with
    | VarDeclStmt (_, name, exp, _) -> (
        let (expc, expp) = trans_exp table frame exp in
        let table' = Symbol.insert name expp table in
        (table', expc, expp)
      )
    | _ -> raise (Failure "only declaration supported")

  let trans_call table frame fn = raise (Failure "TODO trans_call")
end

module InstructionPrinter = struct
  let print_instr (out: out_channel) (inst: ArmInst.inst): unit = (
    Printf.fprintf out "%s\n" (ArmInst.string_of_instr inst)
  )
end

let () =
  let verbose = ref false in
  let filename = ref "" in
  let spec = [
    ("-v", Arg.Set verbose, "verbose output")
  ] in
  Arg.parse spec (fun x -> filename := x) "wacc";
  if (!verbose) then print_string "enable verbose\n" else ();
  print_string !filename; print_newline ();
  let out_file = stdout in
  let code = [
    ArmInst.InstLabel "label1";
    ArmInst.InstDp (ArmInst.Add, ArmInst.Reg 1, ArmInst.Imm 1)]
  in
  List.iter (fun c -> InstructionPrinter.print_instr out_file c) code;
  (close_out out_file)
