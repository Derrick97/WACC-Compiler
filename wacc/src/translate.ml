module S = Symbol;;
module Sem = Semantic;;
module A = Ast;;
open Ast;;
open Arm;;

type frag = unit
type frame = Arm.frame
type access = Arm.access

type enventry = VarEntry of Ast.ty * access

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

let new_frame = Arm.new_frame

let rec zip (a: 'a list) (b: 'b list)
  : (('a * 'b) list) = match (a, b) with
  | ([],_) -> []
  | (_,[]) -> []
  | (x::xs, y::ys) -> ((x,y)::(zip xs ys))

let add_text (ctx: codegen_ctx) (text: string): string = (
  let key = "msg_" ^ (string_of_int ctx.ctx_counter) in
  Hashtbl.add ctx.ctx_text key text;
  key
)

let trans_dec _ = failwith "TODO"

let trans_lit (frame: frame)
    (lit: Ast.literal): access = failwith "TODO"
  (* match lit with *)
  (* | LitBool b -> (if b then OperImm 1 else OperImm 0 ) *)
  (* | LitInt i -> (OperImm i) *)
  (* | LitChar c -> OperImm (Char.code c) *)
  (* | LitString s -> failwith "TODO trans string" *)
  (* | _ -> failwith "TODO lit" *)

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
    (env: enventry Symbol.table)
    (frame: frame)
    (exp: Ast.exp): access = match exp with
  | LiteralExp (lit, _) -> begin
      let temp = allocate_temp frame in
      let i = trans_lit frame lit in
      frame <: (load temp i); temp
    end
  | BinOpExp (lhs, op, rhs, _) -> begin
      let operand_lhs = trans_exp env frame lhs and
          operand_rhs = trans_exp env frame rhs in
      let inst = match op with
        | _ -> assert false in
      operand_lhs
    end
  | IdentExp (name, _) -> begin
      let temp = allocate_temp frame in
      frame <: load temp temp; temp
    end
  | UnOpExp (op, exp, _) -> begin
      let exp_oper = trans_exp env frame exp in
      (match op with
        | NotOp -> assert false
        | NegOp -> failwith "FIXME" (* frame <: SUB (exp_oper, access_of_int 0, exp_oper); exp_oper *)
        | ChrOp -> (trans_call env frame "wacc_chr" [exp])
        | OrdOp -> (trans_call env frame "wacc_ord" [exp])
        | LenOp -> (trans_call env frame "wacc_len" [exp]))
    end
  | CallExp (fname, args, _) -> trans_call env frame fname args
  | ArrayIndexExp _ -> failwith "TODO arrays"
  | NewPairExp _ | FstExp _ | SndExp _ | NullExp _ -> failwith "TODO pairs"

and function_prologue (frame: frame) (args: access list): unit = begin
  (* assert (List.length args < 3); *)
  (* List.iter (fun (x, y) -> ignore(frame <: (mov x (y)))) *)
  (*   (zip args Arm.caller_saved_regs) *)
  end

and function_epilogue (frame: frame)
  : unit = ()

and trans_call
    (env: enventry Symbol.table)
    (frame: frame) (fname: string) (args: exp list): access =
  let args_val = List.map (trans_exp env frame) args in
  function_prologue frame args_val;
  frame <: BL fname;
  function_epilogue frame;
  let (InReg output) as o = allocate_temp frame in
  (frame <: (MOV (output, (OperReg reg_RV))));
  o

let rec trans_stmt
    (env: enventry Symbol.table)
    (frame: frame)
    (stmt: Ast.stmt) : unit = match stmt with
  | VarDeclStmt (ty, name, rhs, _) -> begin
      let rvalue = trans_exp env frame rhs in
      let l = allocate_local frame in
      frame <: (store l rvalue)
    end
  | SeqStmt (stmt, stmtlist) -> (
      trans_stmt env frame stmt;
      trans_stmt env frame stmtlist)
  | AssignStmt (IdentExp (name, _), rhs, _) -> begin
      let rhs = trans_exp env frame rhs in
      let VarEntry (_, acc) = Symbol.lookup name env in
      frame <: (store acc rhs)
    end
  | AssignStmt (lhs, rhs, _) -> failwith "TODO assignment besides identifier"
  | PrintStmt (newline, exp, _) -> begin
      let InReg e = trans_exp env frame exp in
      if newline then
        ignore(frame <: (BL "wacc_println"));
    end
  | _ -> failwith "TODO"
  (*     let k = add_text global_ctx s in *)
  (*     frame <: load (OperReg (Reg 0)) (OperSym k); *)
  (*     frame <: (BL "wacc_print_string"); *)
  (*   end *)
  (* | ExitStmt (exp, _) -> begin *)
  (*     let exp_op = trans_exp frame exp in *)
  (*     frame <: MOV (OperReg (Reg 0), exp_op); *)
  (*     frame <: BL "exit"; *)
  (*     frame <: LDR (OperSym (string_of_int 0), OperReg (Reg 0)) *)
  (*   end *)
  (* | _ -> failwith "TODO other stmt" *)

let trans out stmt =
  let frame = new_frame "main" in
  let env = Symbol.empty in
  let open Printf in
  let () = trans_stmt env frame stmt in
  fprintf out ".data\n";
  Hashtbl.iter
    (fun k v -> (Printf.fprintf out "%s:\n\t.ascii \"%s\" \n" k v);)
    global_ctx.ctx_text;
  fprintf out ".global main\n";
  fprintf out ".text\n";
  print_frame frame out;;

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
