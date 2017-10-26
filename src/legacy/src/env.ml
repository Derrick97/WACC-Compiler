module S = Symbol
open Ast

  (* 4.1.2 might useful *)
  type inst =
    | InstDp of opcode * reg * operand2
    | InstMult of opcode
    | InstSdt
    | InstBr of opcode * string
    | InstSp of inst_sp
    | InstLabel of string
    | InstNoOp
  and inst_sp =
    | Sp_lsl of reg * reg
    | Halt
  and opcode =
    (* Data processing opcodes *)
    | ADD
    | SUB
    | RSB
    | AND
    | EOR
    | ORR
    | MOV
    | TST
    | TEQ
    | CMP
    (* Single Data Transfer opcodes *)
    | LDR
    | STR
    (* Multiply opcodes *)
    | MUL
    | MLA
    (* Branching opcodes *)
    | BEQ
    | BNE
    | BGE
    | BLT
    | BGT
    | BLE
    | B
    | BL
    (* Special opcodes *)
    | LSL
    | ANDEQ
  and operand2 =
    | Rm of reg
    | Imm of int
  and reg = Reg of int
  and shift =
    | ConstShift of int
    | RegShift of reg

  let string_of_opcode code = match code with
    | ADD -> "add"
    | SUB -> "sub"
    | RSB -> "rsb"
    | AND -> "and"
    | EOR -> "eor"
    | ORR -> "orr"
    | MOV -> "mov"
    | TST -> "tst"
    | TEQ -> "teq"
    | CMP -> "cmp"
    | LDR -> "ldr"
    | STR -> "str"
    | MUL -> "mul"
    | MLA -> "mla"
    | BEQ -> "beq"
    | BNE -> "bne"
    | BGE -> "bge"
    | BLT -> "blt"
    | BGT -> "bgt"
    | BLE -> "ble"
    | B -> "b"
    | BL -> "bl"
    | LSL -> "lsl"
    | ANDEQ -> "andeq"
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

let print_instr (out: out_channel) (inst: inst): unit = (
  Printf.fprintf out "%s\n" (string_of_instr inst)
)

type level = int

type access =
  | InMem of int
  | InReg of reg

type frame = {
  mutable counter: int;
  mutable locals: access list;
  level: int;
}

type operand =
  | OperandReg of reg
  | OperandImm of int

let outtermost = 0

let new_frame level = { counter=(-1); locals=[]; level=level}

let allocate_local frame = (
  frame.counter <- frame.counter + 1;
  assert (frame.counter < 13);
  let r = Reg frame.counter in
  ignore(frame.locals = (InReg r)::frame.locals);
  r
)

type codegen_env = access S.table

type codegen_ctx = {
  mutable ctx_counter: int;
  mutable ctx_text: (string, string) Hashtbl.t;
  mutable ctx_data: inst list;
  mutable ctx_env: access Symbol.table;
  mutable ctx_frame: frame;
}

let new_context (): codegen_ctx =
  {
    ctx_counter = 0;
    ctx_text = Hashtbl.create 0;
    ctx_data = [];
    ctx_env = Symbol.empty;
    ctx_frame = new_frame outtermost
  }

let add_text (ctx: codegen_ctx) (text: string): string = (
  let key = "msg_" ^ (string_of_int ctx.ctx_counter) in
  Hashtbl.add ctx.ctx_text key text;
  key
)

let lookup_local ctx name = Symbol.lookup' name ctx.ctx_env

let trans_lit lit = match lit with
  | LitBool b -> (if b then OperandImm 1 else OperandImm 0 )
  | LitInt i -> OperandImm i
  | LitChar c -> OperandImm (Char.code c)
  | _ -> raise (Failure "TODO")

(* Below are WIP *)
(* translation follows the following rule:
   trans(e) = <c, p>
*)
let (++) x y = List.concat [x; y]

let rec trans_exp (ctx: codegen_ctx) (exp: Ast.exp): operand  = match exp with
    | BinOpExp (lhs, op, rhs, _) -> (
        let tr = trans_exp ctx in
        let lhsp = tr lhs in  (*-- *c denotes commands, *p denotes pure expressions, see CMU fp's notes --*)
        let rhsp = tr rhs in
        let o = (match op with
            | PlusOp -> ADD
            | MinusOp -> SUB
            | TimesOp -> MUL
            | _ -> raise (Failure "TODO match op")) in
        let dst = allocate_local ctx.ctx_frame in
        let op2 = (match rhsp with
            | OperandReg r -> Rm r
            | OperandImm num -> Imm num) in
        let inst = InstDp (o, dst, op2) in
        ctx.ctx_data <- (ctx.ctx_data @ [inst]);
        (OperandReg dst)
      )
    | IdentExp (ident, _) -> (
        let InReg r = lookup_local ctx ident in (* FIXME *)
        (OperandReg r)
      )
    | LiteralExp (lit, _) -> trans_lit lit
    | _ -> raise (Failure "TODO other expression")

let rec trans_stmt (ctx: codegen_ctx) (stmt: Ast.stmt) : unit = match stmt with
  | VarDeclStmt (ty,name,exp,_) -> trans_decl ctx stmt
  | SeqStmt [] -> ()
  | SeqStmt (s::ss) -> (
      trans_stmt ctx s;
      trans_stmt ctx (SeqStmt ss))
  | AssignStmt (lhs, rhs, _) -> raise (Failure "TODO assignment")
  | PrintStmt ((LiteralExp (LitString s, _)), _) -> begin
      let l = add_text ctx s in
      ctx.ctx_data <- (ctx.ctx_data @ [InstBr (BL, l)])
    end
  | _ -> raise (Failure "TODO trans_stmt")

and trans_decl (ctx: codegen_ctx) decl = match decl with
  | VarDeclStmt (_, name, exp, _) -> (
      let expp = trans_exp ctx exp in
      let store: access = InReg (allocate_local ctx.ctx_frame) in
      let r = (match store with
          | InReg rr -> rr
          | _ -> raise (Failure "unsupported rvalue access")) in
      let op2 = (match expp with
          | OperandImm i -> Imm i
          | OperandReg r -> Rm r) in
      let store_inst = InstDp (STR, r, op2) in
      ctx.ctx_data <- (ctx.ctx_data @ [store_inst])
    )
  | _ -> raise (Failure "only declaration supported")

let trans_call table frame fn = raise (Failure "TODO trans_call")

let trans stmt =
  let ctx = new_context () in
  let () = trans_stmt ctx stmt in
  List.iter (fun x -> print_instr stderr x) ctx.ctx_data

let () =
  let verbose = ref false in
  let print_ast = ref false in
  let filename = ref "" in
  let spec = [
    ("-v", Arg.Set verbose, "verbose output");
    ("--print-ast", Arg.Set print_ast, "print the AST")
  ] in
  Arg.parse spec (fun x -> filename := x) "wacc";
  let lexbuf = Lexing.from_channel (open_in !filename) in
  let (decs, ast) = Parser.prog Lexer.main lexbuf in
  if (!print_ast) then (
    print_string "-----AST output-----\n";
    print_string (Ast.pp_stmt ast); print_newline ();
    print_string "-----end output-----\n");
  let out_file = stdout in
  let () = trans ast in
  (close_out out_file)
