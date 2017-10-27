module S = Symbol
open Ast

  (* 4.1.2 might useful *)
type inst =
  | InstDp    of opcode * reg * operand2
  | InstSdt   of opcode * reg * offset (* op * rn * rs *)
  | InstBr    of opcode * label
  | InstLabel of label
  | InstNoOp
  | InstTHUMB of opcode * reg list        (* for stuff such as push/pop *)
and label = string
and offset =
  | OffsetImm of reg * int
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
  | PUSH
  | POP
and reg = Reg of int
and shift =
  | ShiftImm of int
  (* | ShiftReg of reg             (\* TODO support this *\) *)
and rotate = int
and operand2 =
  | Operand2Imm of rotate * int    (* rotate * imm *)
  (* | Operand2Reg of shift * reg  TODO *)
and access =
  | InMem of int
  | InReg of reg
  | InImm of int
  | InUndet of int
and memory_address =
  | MemAddr

let rFP = Reg 13                 (* frame pointer FP *)
let rLR = Reg 14                 (* link register LR *)

type level = int

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
  | PUSH -> "push"
  | POP -> "pop"
let string_of_reg reg = match reg with
  | Reg 13 -> "rsp"           (* Stack pointer *)
  | Reg 14 -> "LR"           (* Link register *)
  | Reg 15 -> "PC"            (* Program counter *)
  | Reg 16 -> "aspr"
  | Reg r -> (if (r < 0 || r > 16) then
                raise (Invalid_argument "Not a valid register number")
              else "r" ^ (string_of_int r))
let string_of_shift (s: shift) = match s with
  | ShiftImm i -> "#" ^ (string_of_int i)
let string_of_operand2 (op2: operand2) = match op2 with
  | Operand2Imm (rotate, imm) -> "#" ^ (string_of_int imm)

let string_of_offset (offset: offset) = match offset with
  | OffsetImm (rm, i) -> "[" ^ (string_of_reg rm) ^ "]"
  | _ -> failwith "TODO string_of_offset"

let string_of_instr instr = match instr with
  | InstDp (op, reg, op2) -> (string_of_opcode op) ^ ", " ^ (string_of_reg reg) ^ ", " ^ (string_of_operand2 op2)
  | InstLabel label -> label ^ ":"
  | InstBr (cond, label) -> (string_of_opcode cond) ^ " " ^ label
  | InstSdt (op, rd, offset) -> (string_of_opcode op) ^ " " ^
                                (string_of_reg rd) ^ "," ^
                                (string_of_offset offset) (* see 4-32 *)
  | InstTHUMB (op, r::rs) -> (string_of_opcode op) ^ " {" ^ (string_of_reg r)  ^ "}"
  | _ -> ""


let print_global (out: out_channel) =
  Printf.fprintf out ".global main\n"

let print_instr
    (out: out_channel)
    (inst: inst): unit = (
  Printf.fprintf out "%s\n" (string_of_instr inst))

(* Activation record (AR) *)
type frame = {
  mutable name: string;
  mutable counter: int;         (* used for generating locals *)
  mutable locals: (string, access) Hashtbl.t;
  mutable instructions: inst array;
  level: int;
}

let outtermost = 0

let new_frame (name:string)
    level = { name = name;
              counter=(-1);
              locals= Hashtbl.create 0;
              level=level;
              instructions=[| |]}

let allocate_local
  (frame: frame): access  = (
  frame.counter <- frame.counter + 1;
  let r = InMem frame.counter in
  Hashtbl.add frame.locals (string_of_int frame.counter) r;
  r)

let allocate_temp
  (frame: frame): access = (
  frame.counter <- frame.counter + 1;
  assert (frame.counter < 13);
  let r = InReg (Reg frame.counter) in
  Hashtbl.add frame.locals (string_of_int frame.counter) r;
  r)

type codegen_ctx = {
  mutable ctx_counter: int;
  mutable ctx_text: (string, string) Hashtbl.t;
  mutable ctx_env: access Symbol.table;
  mutable ctx_frames: frame array;
  mutable ctx_frame: frame;
}

let new_context (): codegen_ctx = {
    ctx_counter = 0;
    ctx_text = Hashtbl.create 0;
    ctx_env = Symbol.empty;
    ctx_frames = [| |];
    ctx_frame = new_frame "main" outtermost;
  }

let add_text (ctx: codegen_ctx) (text: string): string = (
  let key = "msg_" ^ (string_of_int ctx.ctx_counter) in
  Hashtbl.add ctx.ctx_text key text;
  key
)

let lookup_local ctx name = Symbol.lookup' name ctx.ctx_env

let codegen_frame (f: frame)
  : inst list =
  [ InstLabel f.name;
    InstTHUMB (PUSH, [rLR]); ] @
  (* codegen stack variable allocation *)
  Array.to_list f.instructions @
  [ InstTHUMB (POP, [rLR]);  ]

let trans_lit
    (ctx: codegen_ctx)
    (lit: Ast.literal): access = match lit with
  | LitBool b -> (if b then InImm 1 else InImm 0 )
  | LitInt i -> (InImm i)
  | LitChar c -> InImm (Char.code c)
  | LitString s -> failwith "TODO trans string"
  | _ -> failwith "TODO lit"


let (<:) (frame: frame) (i) = (
  ignore(frame.instructions <-
         Array.append frame.instructions [| i |]); ())

let mov
    (dst: access)
    (src: access): inst = match (dst, src) with
  | (InReg r, InImm v) -> InstDp (MOV, r, Operand2Imm (0, v)) (* FIXME assume rotate = 0 *)
  | (InMem m, InReg r) -> failwith "TODO mov"
  | _ -> failwith "TODO mov"


let store (dst: access) (src: access) = match (dst, src) with
  | (InMem d, InReg s) -> InstSdt (STR, s, OffsetImm (s, 0))
  | _ -> failwith "TODO store"

let load (dst: access) (src: access) = failwith "todo load"

let rec trans_exp
    (ctx: codegen_ctx)
    (exp: Ast.exp): access = match exp with
  | LiteralExp (lit, _) ->
    let temp = allocate_temp ctx.ctx_frame in
    let l = (trans_lit ctx lit) in
    ctx.ctx_frame <: (mov temp l);
    temp
  | BinOpExp (lhs, op, rhs, _) -> failwith "TODO exp"
  | _ -> failwith "TODO exp"

let rec trans_stmt
    (ctx: codegen_ctx)
    (stmt: Ast.stmt) : unit = match stmt with
  | VarDeclStmt (ty,name,exp,_) -> trans_var_decl ctx stmt
  | SeqStmt (stmt, stmtlist) -> (
      trans_stmt ctx stmt;
      trans_stmt ctx stmtlist)
  | AssignStmt (lhs, rhs, _) -> failwith "TODO assignment"
  | PrintStmt (s,_) -> trans_print_stmt ctx stmt
  | _ -> failwith "TODO trans_stmt"

and trans_print_stmt
    (ctx: codegen_ctx)
    (stmt: Ast.stmt) = failwith "TODO trans_print others"

and trans_var_decl
    (ctx: codegen_ctx)
    (decl: Ast.stmt) = match decl with
  | VarDeclStmt (_,name,rhs,_) -> begin
      let rvalue = trans_exp ctx rhs in
      let l = allocate_local ctx.ctx_frame in
      ctx.ctx_frame <: (store l rvalue)   (* FIXME *)
    end
  | _ -> assert false

let func_prologue (ctx: codegen_ctx) = ()
let func_epilogue (ctx: codegen_ctx) = ()

let trans_call (ctx: codegen_ctx) fn = match fn with
  | CallExp (fname, args, _) ->
    begin
      let args_val = List.map (trans_exp ctx) args in
      assert (List.length args_val < 3); (* more arguments *)
    end
  | _ -> failwith "should not occur"

let trans stmt =
  let out = stderr in
  let ctx = new_context () in
  let () = trans_stmt ctx stmt in
  print_global out;
  List.iter (fun x -> match x with
      | InstLabel _ -> print_instr out x
      | _ -> (Printf.fprintf out "\t";
              print_instr out x) ) (codegen_frame ctx.ctx_frame)

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
    print_string (Prettyprint.prettyprint_stmt ast); print_newline ();
    print_string "-----end output-----\n");
  let out_file = stdout in
  let () = trans ast in
  (close_out out_file)
