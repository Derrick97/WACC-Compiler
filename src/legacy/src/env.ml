module S = Symbol
open Ast

  (* 4.1.2 might useful *)
type inst =
  | InstDp    of opcode * reg * operand2
  | InstSdt   of opcode * reg * access option
  | InstBr    of opcode * string
  | InstSp    of inst_sp
  | InstLabel of string
  | InstNoOp
and inst_sp =
  | Sp_lsl of reg * reg
  | Halt
and operand =
  | OperandReg of reg
  | OperandImm of int
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
and access =
  | InMem of int
  | InReg of reg
  | InImm of int

let fp = Reg 13                 (* frame pointer FP *)
let lr = Reg 14                 (* link register LR *)

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
  | Reg 14 -> "rlr"           (* Link register *)
  | Reg 15 -> "pc"            (* Program counter *)
  | Reg 16 -> "aspr"
  | Reg r -> (if (r < 0 || r > 16) then
                raise (Invalid_argument "Not a valid register number")
              else "r" ^ (string_of_int r))
let string_of_shift s = match s with
  | _ -> "shift"
let string_of_operand op2 = match op2 with
  | OperandReg reg -> "Rm" ^ (string_of_reg reg)
  | OperandImm i -> "#" ^ (string_of_int i)
let string_of_instr instr = match instr with
  | InstDp (op, reg, operand) -> (string_of_opcode op) ^ " " ^ (string_of_reg reg) ^ " " ^ (string_of_operand operand)
  | InstLabel label -> label ^ ":"
  | InstBr (cond, label) -> (string_of_opcode cond) ^ " " ^ label
  | _ -> "TODO"

let print_instr (out: out_channel) (inst: inst): unit = (
  Printf.fprintf out "%s\n" (string_of_instr inst)
)

(* Activation record (AR) *)
type frame = {
  mutable counter: int;         (* used for generating locals *)
  mutable locals: access list;
  mutable instructions: inst list;
  level: int;
}

let outtermost = 0

let new_frame level = { counter=(-1);
                        locals=[];
                        level=level;
                        instructions=[]}

let allocate_local
  (frame: frame): access  = (
  frame.counter <- frame.counter + 1;
  assert (frame.counter < 13);
  let r = InMem frame.counter in
  ignore(frame.locals = r::frame.locals);
  r
)

let allocate_temp
  (frame: frame): access = (
  frame.counter <- frame.counter + 1;
  assert (frame.counter < 13);
  let r = InReg (Reg frame.counter) in
  ignore(frame.locals = r::frame.locals);
  r)

type codegen_ctx = {
  mutable ctx_counter: int;
  mutable ctx_text: (string, string) Hashtbl.t;
  mutable ctx_data: inst list;
  mutable ctx_env: access Symbol.table;
  mutable ctx_frames: frame array;
  mutable ctx_frame: frame;
}

let new_context (): codegen_ctx =
  {
    ctx_counter = 0;
    ctx_text = Hashtbl.create 0;
    ctx_data = [];
    ctx_env = Symbol.empty;
    ctx_frames = [| |];
    ctx_frame = new_frame outtermost;
  }

let add_text (ctx: codegen_ctx) (text: string): string = (
  let key = "msg_" ^ (string_of_int ctx.ctx_counter) in
  Hashtbl.add ctx.ctx_text key text;
  key
)

let lookup_local ctx name = Symbol.lookup' name ctx.ctx_env

let trans_lit (lit: Ast.literal): access = match lit with
  | LitBool b -> (if b then InImm 1 else InImm 0 )
  | LitInt i -> (InImm i)
  | LitChar c -> InImm (Char.code c)
  | _ -> raise (Failure "TODO")


let (++) x y = List.concat [x; y]

let rec trans_exp
    (ctx: codegen_ctx)
    (exp: Ast.exp): access = match exp with
    | BinOpExp (lhs, op, rhs, _) -> (
        let tr = trans_exp ctx in
        let InReg lhsp = tr lhs in (* todo handle in memory *)
        let InReg rhsp = tr rhs in
        let InReg dst = allocate_temp ctx.ctx_frame in
        let o = (match op with
            | PlusOp -> ADD
            | MinusOp -> SUB
            | TimesOp -> MUL
            | _ -> raise (Failure "TODO match op")) in
        let insts = [
          InstSdt (STR, dst, Some (InReg lhsp)); (* TODO check this *)
          InstDp (o, dst, OperandReg rhsp)
        ] in
        ctx.ctx_data <- (ctx.ctx_data @ insts);
        InReg dst
      )
    | IdentExp (ident, _) -> (
        let v = lookup_local ctx ident in
        let InReg t = allocate_temp ctx.ctx_frame in
        ctx.ctx_data <- (ctx.ctx_data @ [InstSdt (LDR, t, Some v)]);
        InReg t (* FIXME *))
    | LiteralExp (lit, _) -> trans_lit lit
    | _ -> raise (Failure "TODO other expression")

let rec trans_stmt
    (ctx: codegen_ctx)
    (stmt: Ast.stmt) : unit = match stmt with
  | VarDeclStmt (ty,name,exp,_) -> trans_var_decl ctx stmt
  | SeqStmt [] -> ()
  | SeqStmt (s::ss) -> (
      trans_stmt ctx s;
      trans_stmt ctx (SeqStmt ss))
  | AssignStmt (lhs, rhs, _) -> failwith "TODO assignment"
  | PrintStmt (s,_) -> trans_print_stmt ctx stmt
  | _ -> failwith "TODO trans_stmt"

and trans_print_stmt
    (ctx: codegen_ctx)
    (stmt: Ast.stmt) = match stmt with
  | PrintStmt ((LiteralExp (LitString s, _)), _) -> begin
      let l = add_text ctx s in
      let insts = [
        InstBr (BL, l);
      ] in
      ctx.ctx_data <- (ctx.ctx_data @ insts)
    end
  | _ -> failwith "TODO trans_print others"

and trans_var_decl
    (ctx: codegen_ctx)
    (decl: Ast.stmt) = match decl with
  | VarDeclStmt (_, name, exp, _) -> (
      let InReg expp = (match trans_exp ctx exp with
          | InReg r -> InReg r
          | InImm r -> (let InReg t = allocate_temp ctx.ctx_frame in
                        ctx.ctx_data <- (ctx.ctx_data @ [InstSdt (STR, t, Some (InImm r))]);
                        InReg t)
          | _ -> failwith "should not occur") in
      let store = (allocate_local ctx.ctx_frame) in
      let inst = InstSdt (STR, expp, Some (InReg expp)) in
      ctx.ctx_data <- (ctx.ctx_data @ [inst]))
  | _ -> failwith "only variable declaration supported"

let func_prologue (ctx: codegen_ctx) = ()
let func_epilogue (ctx: codegen_ctx) = ()

let trans_func_decl () = failwith "TODO func_decl"
    (* (ctx: codegen_ctx)
   *   (f: Ast.dec): frame = match f with
   * | FuncDec (t, fname, args, body, _) ->
   *   begin
   *     let frame = new_frame (ctx.ctx_frame.level + 1) in
   *     let insts = [
   *       InstLabel (Symbol.string_of_symbol fname);
   *       InstSdt (PUSH, fp, None);
   *       InstSdt (PUSH, lr, None);
   *       (\* the body *\)
   *       InstSdt (POP, fp, None);
   *       InstSdt (POP, lr, None);
   *     ] in
   *     frame
   *   end *)

let trans_call (ctx: codegen_ctx) fn = match fn with
  | CallExp (fname, args, _) ->
    begin
      let args_val = List.map (trans_exp ctx) args in
      assert (List.length args_val < 3); (* more arguments *)
      let insts = List.mapi (fun i x -> InstSdt (LDR, Reg i, None)) args_val in
      let insts = insts @ [InstBr (BL, fname)] in (* Clean up ? *)
      ctx.ctx_data <- ctx.ctx_data @ insts
    end
  | _ -> failwith "should not occur"

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
