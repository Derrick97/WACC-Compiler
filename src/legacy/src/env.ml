module Bindings = struct
  type t = string
  let compare = String.compare
end;;

module Environment = Map.Make(Bindings);;

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

(* Tree is an IR *)
module Tree: sig
  type exp
  type stmt
  type binop
  type relop
end = struct
  type binop = PLUE | MINUS | MUL | DIV | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR
  and relop = EQ | NE | LT | GT | LE | GE | ULT | UGT | UGE
  and exp =
    | Const of int
    | Name of Temp.label
    | Temp of Temp.temp
    | Binop of binop * exp * exp
    | Mem of exp
    | Eseq of stmt * exp
  and stmt =
    | Move of exp * exp
    | Exp of exp
    | Jump of exp * Temp.label list
    | Seq of stmt * stmt
    | Label of Temp.label
end


module Frame: sig
  (* We use a frame to organize codegen *)
  type frame
  type level
  type access

  val new_frame: level -> frame
  val formals: frame -> unit
  val allocate_local: frame -> access
  val outtermost: level
  val trans_exp: frame -> Tree.exp -> unit

end = struct
  type level = int

  type access =
    | InMem of int
    | InReg of int

  type frame = {
    mutable counter: int;
    mutable locals: access list;
    level: int;
  }

  (* Type representing ARM instructions *)
  type inst =
    | Label

  let outtermost = 0
  let static_links = ref []

  let new_frame level = { counter=(-1); locals=[]; level=level}
  let formals frame = ()

  let allocate_local frame = (
    frame.counter <- frame.counter + 1;
    assert (frame.counter < 13);
    InReg frame.counter
  )

  let trans_lit lit = match lit with
    | LitString s -> ArmInst.Imm 0
    | LitBool b -> (if b then ArmInst.Imm 1 else ArmInst.Imm 0 )
    | LitInt i -> ArmInst.Imm 1
    | LitChar c -> ArmInst.Imm (ord c)

  let trans_exp frame exp = match exp with
    | BinOpExp (lhs, op, rhs, _) -> (
        let lhse = trans_exp frame lhs in
        let rhse = trans_exp frame rhs in
        let o = (match op with
        | PlusOp -> PLUS
        | MinusOp -> MINUS
        | TimesOp -> MUL
        | DivideOp -> DIV) in
        ())
  let trans_stmt frame exp = ()
  let trans_decl frame decl = ()
  let trans_call frame fn = ()
end

module InstructionPrinter = struct
  let print_instr (out: out_channel) (inst: ArmInst.inst): unit = (
    Printf.fprintf out "%s\n" (ArmInst.string_of_instr inst)
  )
end

let () =
  let out_file = (open_out "foo.txt") in
  let code = [
    ArmInst.InstLabel "label1";
    ArmInst.InstDp (ArmInst.Add, ArmInst.Reg 1, ArmInst.Imm 1)]
  in
  List.iter (fun c -> InstructionPrinter.print_instr out_file c) code;
  (close_out out_file)
