module S = Symbol
open Ast;;
open Asm;;

type entry' = Entry of ty * operand

(* Activation record (AR) *)
type frame = {
  mutable name: string;
  mutable counter: int;         (* used for generating locals *)
  mutable offset: int;
  mutable locals: (string, entry') Hashtbl.t;
  mutable temps: entry' array;
  mutable instructions: inst array;
  level: int;
}

let outermost = 0

let new_frame
    (name:string) level = { name = name;
                            counter=(4);
                            offset=0;
                            locals= Hashtbl.create 0;
                            temps=[| |];
                            level=level;
                            instructions=[| |]}

let allocate_local
    (frame: frame)
    (t: ty)
    (name: string): operand  = (
  let r = OperAddr (Addr (reg_SP, frame.offset)) in
  let size = match t with
    | IntTy -> 4
    | CharTy -> 1
    | BoolTy -> 1
    | _ -> assert false in
  frame.offset <- frame.offset + size;
  Hashtbl.add frame.locals name (Entry (t, r));
  r)

let allocate_temp
    (frame: frame)
    (t: ty): operand = (
  assert (frame.counter < 13);
  let r = OperReg (Reg frame.counter) in
  frame.counter <- frame.counter + 1;
  ignore(Array.append frame.temps [| (Entry (t, r)) |]);
  r)

let find_local (frame: frame) (name: string) =
  Hashtbl.find frame.locals name

type codegen_ctx = {
  mutable ctx_counter: int;
  mutable ctx_text: (string, string) Hashtbl.t;
  mutable ctx_frames: frame array;
}

let new_context (): codegen_ctx = {
    ctx_counter = 0;
    ctx_text = Hashtbl.create 0;
    ctx_frames = [| |];
}

let global_ctx = new_context ()

let add_text (ctx: codegen_ctx) (text: string): string = (
  let key = "msg_" ^ (string_of_int ctx.ctx_counter) in
  Hashtbl.add ctx.ctx_text key text;
  key
)

let trans_lit
    (frame: frame)
    (lit: Ast.literal): operand = match lit with
  | LitBool b -> (if b then OperImm 1 else OperImm 0 )
  | LitInt i -> (OperImm i)
  | LitChar c -> OperImm (Char.code c)
  | LitString s -> failwith "TODO trans string"
  | _ -> failwith "TODO lit"

let (<:) (frame: frame) (inst): unit = (
  ignore(frame.instructions <- Array.append frame.instructions [| inst |]); ())

let mov (dst: operand) (src: operand): inst =
  MOV (dst, src)

let store (dst: operand) (src: operand) =
  STR (src, dst);;

let load (dst: operand) (src: operand) =
  LDR (src, dst);;

let binop_to_asm = function
  | PlusOp -> fun x y z -> ADD (x, y, z)
  | MinusOp -> fun x y z -> SUB (x, y, z)
  | AndOp -> fun x y z -> AND (x, y, z)
  | OrOp -> fun x y z -> ORR (x, y, z)
  | _ -> assert false

let type_of_lit = function
  | Ast.LitInt  _ -> IntTy
  | Ast.LitChar _ -> CharTy
  | Ast.LitBool _ -> BoolTy
  | _ -> assert false

let size_of_ty = function
  | IntTy -> 4
  | CharTy -> 1
  | BoolTy -> 1
  | _ -> assert false

let rec trans_exp
    (frame: frame)
    (exp: Ast.exp): operand = match exp with
  | LiteralExp (lit, _) -> begin
      let temp = allocate_temp frame (type_of_lit lit) in
      let OperImm i = trans_lit frame lit in
      frame <: (load temp (OperSym (string_of_int i))); temp
    end
  | BinOpExp (lhs, op, rhs, _) -> begin
      let operand_lhs = trans_exp frame lhs and
          operand_rhs = trans_exp frame rhs in
      match op with
      | (PlusOp | MinusOp | AndOp | OrOp) as o ->
        frame <: (binop_to_asm o) operand_lhs operand_rhs operand_lhs;
                 operand_lhs
      | _ -> assert false
    end
  | IdentExp (name, _) -> begin
      let Entry (ty, operand) = find_local frame name in
      let temp = allocate_temp frame ty in
      frame <: load temp operand; temp
    end
  | UnOpExp (op, exp, _) -> begin
      let exp_oper = trans_exp frame exp in
      (match op with
        | NotOp -> assert false
        | NegOp -> frame <: SUB (exp_oper, OperImm 0, exp_oper); exp_oper
        | ChrOp -> (trans_call frame "wacc_chr" [exp])
        | OrdOp -> (trans_call frame "wacc_ord" [exp])
        | LenOp -> (trans_call frame "wacc_len" [exp]))
    end
  | CallExp (fname, args, _) -> trans_call frame fname args
  | ArrayIndexExp _ -> failwith "TODO arrays"
  | NewPairExp _ | FstExp _ | SndExp _ | NullExp _ -> failwith "TODO pairs"


and trans_call (frame: frame) (fname: string) (args: exp list): operand =
  let args_val: operand list = List.map (trans_exp frame) args in
  assert (List.length args_val < 3); (* more arguments *)
  (* TODO *)
  frame <: BL fname;
  allocate_temp frame IntTy

let rec trans_stmt
    (frame: frame)
    (stmt: Ast.stmt) : unit =
  let trans_var_decl (decl: Ast.stmt) = match decl with
    | VarDeclStmt (ty,name,rhs,_) -> begin
        let rvalue = trans_exp frame rhs in
        let l = allocate_local frame ty name in
        frame <: (store l rvalue)
      end
    | _ -> assert false in
  match stmt with
  | VarDeclStmt (ty,name,exp,_) -> trans_var_decl stmt
  | SeqStmt (stmt, stmtlist) -> (
      trans_stmt frame stmt;
      trans_stmt frame stmtlist)
  | AssignStmt (IdentExp (name, _), rhs, _) -> begin
      let roperand = trans_exp frame rhs in
      let Entry (_, loperand) = find_local frame name in
      frame <: store roperand loperand;
    end
  | AssignStmt (lhs, rhs, _) -> failwith "TODO assignment besides identifier"
  | PrintLnStmt (LiteralExp (LitString s, _), _) | PrintStmt (LiteralExp (LitString s, _), _) -> begin
      let k = add_text global_ctx s in
      frame <: load (OperReg (Reg 0)) (OperSym k);
      frame <: (BL "wacc_print_string");
    end
  | ExitStmt (exp, _) -> begin
      let exp_op = trans_exp frame exp in
      frame <: MOV (OperReg (Reg 0), exp_op);
      frame <: BL "exit";
      frame <: LDR (OperSym (string_of_int 0), OperReg (Reg 0))
    end
  | _ -> failwith "TODO other stmt"

let print_frame (out: out_channel) (frame: frame): unit =
  let open Printf in
  fprintf out "%s\n" (frame.name ^ ":");
  let stack_size = ((Hashtbl.length frame.locals)*4) in
  let push    = PUSH ([(OperReg reg_PC)]) in
  let alloc   = SUB (OperReg reg_SP, OperReg reg_SP, OperImm stack_size) in
  let dealloc = ADD (OperReg reg_SP, OperReg reg_SP, OperImm stack_size) in
  let pop     = POP ([(OperReg reg_PC)]) in
  let insts = [push;            (* push the link register *)
               alloc] @         (* allocate stack locals *)
              (Array.to_list frame.instructions) @
              [dealloc;         (* deallocate stack locals *)
               pop]             (* pop the link register, return *)
  in
  List.iter (fun x -> fprintf out "\t%s\n" (string_of_inst x)) insts

let trans out stmt =
  let frame = new_frame "main" outermost in
  let open Printf in
  let () = trans_stmt frame stmt in
  fprintf out ".data\n";
  Hashtbl.iter
    (fun k v -> (Printf.fprintf out "%s:\n\t.ascii \"%s\" \n" k v);)
    global_ctx.ctx_text;
  fprintf out ".global main\n";
  fprintf out ".text\n";
  print_frame out frame;;

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
  let outpath = Filename.chop_extension (!filename) ^ ".s" in
  let out_file = open_out outpath in
  let () = trans out_file ast in
  (close_out out_file);
  let _ = Sys.command ("cat wacclib.s >> " ^ outpath) in ()
