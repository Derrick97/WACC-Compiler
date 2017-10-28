module S = Symbol
open Ast;;
open Asm;;

(* Activation record (AR) *)
type frame = {
  mutable name: string;
  mutable counter: int;         (* used for generating locals *)
  mutable locals: (string, access) Hashtbl.t;
  mutable temps: access array;
  mutable instructions: inst array;
  level: int;
}

let outermost = 0

let new_frame (name:string)
    level = { name = name;
              counter=(-1);
              locals= Hashtbl.create 0;
              temps=[| |];
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
    ctx_frame = new_frame "main" outermost;
}

let add_text (ctx: codegen_ctx) (text: string): string = (
  let key = "msg_" ^ (string_of_int ctx.ctx_counter) in
  Hashtbl.add ctx.ctx_text key text;
  key
)

let lookup_local ctx name = Symbol.lookup' name ctx.ctx_env

let trans_lit
    (frame: frame)
    (lit: Ast.literal): access = match lit with
  | LitBool b -> (if b then InImm 1 else InImm 0 )
  | LitInt i -> (InImm i)
  | LitChar c -> InImm (Char.code c)
  | LitString s -> failwith "TODO trans string"
  | _ -> failwith "TODO lit"


let (<:) (frame: frame) (inst) = (
  ignore(frame.instructions <-
         Array.append frame.instructions [| inst |]); ())

let mov (dst: access) (src: access): inst =
  Inst_mov {mov_src=src; mov_dst=dst}

let store (dst: access) (src: access) =
  Inst_oper {oper_op=STR;
             oper_src=[src];
             oper_dst=[dst]}

let load (dst: access) (src: access) =
  Inst_oper {oper_op=LDR;
             oper_src=[src];
             oper_dst=[dst]}

let binop_to_asm_operand = function
  | PlusOp -> ADD
  | MinusOp -> SUB
  | _ -> assert false

let rec trans_exp
    (frame: frame)
    (exp: Ast.exp): access = match exp with
  | LiteralExp (lit, _) ->
    let temp = allocate_temp frame in
    frame <: (mov temp (trans_lit frame lit)); temp
  | BinOpExp (lhs, op, rhs, _) -> begin
      let access_lhs = trans_exp frame lhs and
          access_rhs = trans_exp frame rhs in
      match op with
      | (PlusOp | MinusOp) as o ->
        frame <: (Inst_oper {oper_op=(binop_to_asm_operand o);
                             oper_dst=[access_lhs];
                             oper_src=[access_rhs]}); access_lhs
      | _ -> assert false
    end
  | _ -> failwith "TODO exp"

let global_ctx = new_context ()
let rec trans_stmt
    (frame: frame)
    (stmt: Ast.stmt) : unit =
  let trans_call fn =  match fn with
    | CallExp (fname, args, _) -> begin
        let args_val: access list = List.map (trans_exp frame) args in
        let insts = List.mapi (fun i x -> mov (InReg (Reg i)) (x)) in
        assert (List.length args_val < 3); (* more arguments *)
      end
    | _ -> assert false in
  let trans_var_decl (decl: Ast.stmt) = match decl with
    | VarDeclStmt (_,name,rhs,_) -> begin
        let rvalue = trans_exp frame rhs in
        let l = allocate_local frame in
        frame <: (store l rvalue)
      end
    | _ -> assert false in
  let trans_call_external (name:string) (args: access list) = () in
  match stmt with
  | VarDeclStmt (ty,name,exp,_) -> trans_var_decl stmt
  | SeqStmt (stmt, stmtlist) -> (
      trans_stmt frame stmt;
      trans_stmt frame stmtlist)
  | AssignStmt (lhs, rhs, _) -> failwith "TODO assignment"
  | PrintLnStmt (LiteralExp (LitString s, _), _) | PrintStmt (LiteralExp (LitString s, _), _) -> begin
      let k = add_text global_ctx s in
      frame <: load (InReg (Reg 0)) (InLabel k);
      frame <: (Inst_jump "wacc_print_string");
    end
  | _ -> failwith "TODO trans_stmt"

let print_frame (out: out_channel) (frame: frame): unit =
  let open Printf in
  fprintf out "%s\n" (frame.name ^ ":");
  let push = (Inst_oper {oper_op=PUSH;
                         oper_dst=[InReg rLR];
                         oper_src=[]}) in
  let pop = (Inst_oper {oper_op=POP;
                        oper_dst=[InReg rPC];
                        oper_src=[]}) in
  let insts = [push] @ (Array.to_list frame.instructions) @ [pop] in
  List.iter (fun x -> fprintf out "\t%s\n" (string_of_instr x)) insts

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
