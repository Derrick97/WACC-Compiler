open Lexing
open Printf
open Stack

module A = Ast

let usage = "The WACC compiler\nUsage:\n"
let syntax_error_code = 100
let semantic_error_code = 200
type wrapType =
  | Int of int
  | Char of char
  | Bool of bool
  | String of string
  | NullType
  | ListOfElem of A.exp list
  | DefaultTy
let stack: wrapType Stack.t = (Stack.create ())
let setup_main = false
let print_ast = true
let table = ref (Symbol.empty)


let printWrap wrapTy =
  match wrapTy with
  | Int(num) -> print_int num
  | Char(letter) -> print_char letter
  | Bool(tf) -> if tf then print_string "true" else print_string "false"
  | String(str) -> print_string str
  | NullType -> print_string "nil"
  | DefaultTy -> print_string "Something goes wrong.."

let arithmeticConvert wrapTy =
  match wrapTy with
  | Int(num) -> num
  | _ -> -1                   (*TODO:Need Revision here to make it throw an exception*)

let booleanConvert wrapTy =
  match wrapTy with
  | Bool(tf) -> tf
  | _ -> false



(* helper function to get the linum and colnum of the position in a tuple *)
let pos_lnum_cnum pos =
  let lnum = pos.pos_lnum in
  let cnum = (pos.pos_cnum - pos.pos_bol + 1) in
  (lnum, cnum)

(*Test Code*)
      let rec compute exp table = match exp with
        | A.IdentExp (symbol, _) -> compute (Symbol.lookup symbol !table) table
        | A.LiteralExp(lite, _) -> (match lite with
              | A.LitString(str) -> String(str)
              | A.LitBool(boo) -> Bool(boo)
              | A.LitChar(cha) -> Char(cha)
              | A.LitInt (num) -> Int(num)
              | A.LitArray (expList) -> ListOfElem(expList)
              | A.LitPair (ex1, ex2) -> DefaultTy)
        | A.BinOpExp (ex1, binop, ex2, _) -> (match binop with
              | A.PlusOp -> Int(arithmeticConvert (compute ex1 table) + arithmeticConvert(compute ex2 table))
              | A.MinusOp -> Int(arithmeticConvert (compute ex1 table) - arithmeticConvert(compute ex2 table))
              | A.TimesOp -> Int(arithmeticConvert (compute ex1 table) / arithmeticConvert(compute ex2 table))
              | A.DivideOp -> Int(arithmeticConvert (compute ex1 table) / arithmeticConvert(compute ex2 table))
              | A.GeOp -> Bool (arithmeticConvert (compute ex1 table) >= arithmeticConvert(compute ex2 table))
              | A.GtOp -> Bool (arithmeticConvert (compute ex1 table) > arithmeticConvert(compute ex2 table))
              | A.EqOp -> Bool (arithmeticConvert (compute ex1 table) == arithmeticConvert(compute ex2 table))
              | A.NeOp -> Bool (arithmeticConvert (compute ex1 table) != arithmeticConvert(compute ex2 table))
              | A.LtOp -> Bool (arithmeticConvert (compute ex1 table) < arithmeticConvert(compute ex2 table))
              | A.LeOp -> Bool (arithmeticConvert (compute ex1 table) <= arithmeticConvert(compute ex2 table))
              | A.AndOp -> Bool (booleanConvert (compute ex1 table) && booleanConvert(compute ex2 table))
              | A.OrOp -> Bool (booleanConvert (compute ex1 table)|| booleanConvert(compute ex2 table))
              | A.ModOp -> Int(arithmeticConvert (compute ex1 table) mod arithmeticConvert(compute ex2 table)))
        (*| A.ArrayIndexExp(symbol, explist,_) -> (
              match (compute (Symbol.lookup symbol !table) table) with
              | ListOfElem(elems) -> elems;
              let elem = evalList elems explist in
              compute elem table;
            )

      and evalList someList explist = (match explist with
        | exp::[] -> List.nth someList (arithmeticConvert (compute exp table))
        | h::r -> List.nth (evalList someList explist) (List.nth someList (arithmeticConvert compute h table) ) )*)

      let rec matchLHS (lhs: A.exp): string = match lhs with
      | A.IdentExp(symbol, _) -> symbol
      | A.FstExp(exp, _) -> (match exp with
        | A.IdentExp(s,_) -> ("fst" ^ (s))
        | A.ArrayIndexExp(symbol,_,_) -> ("fst[]" ^ (symbol))
        | A.FstExp(exps,_) -> matchLHS exps
        | A.SndExp(exps,_) -> matchLHS exps
        | _ -> Symbol.symbol "what"
        )
      | A.SndExp(exp,_) -> (match exp with
        | A.IdentExp(symbol,_) -> ("snd" ^ (symbol))
        | A.ArrayIndexExp(symbol,_,_) -> ("snd[]" ^ symbol))
        | A.FstExp(exps,_) -> matchLHS exps
        | A.SndExp(exps,_) -> matchLHS exps
        | _ -> Symbol.symbol "what"



let handle_syntax_error lexbuf =
  let pos = lexbuf.lex_start_p in
    fprintf stderr "Syntax error\n";
    fprintf stderr "Near token \"%s\"\n" (Lexing.lexeme lexbuf);
    fprintf stderr "Near %d:%d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1);
    exit(syntax_error_code)

let rec add_func_declarations table = function
  | [] -> table
  | (f::fs) -> begin
      match f with
      | A.FuncDec (ty, ident, fields, stmt, pos) ->
        let tys = List.map fst fields in
        let table' = Symbol.insert ident (Semantic.FuncEntry (ty, tys)) table in
        let table'' = List.fold_left (fun table (ty, ident) -> Symbol.insert ident (Semantic.VarEntry ty) table) table' fields in
        (* ignore(Semantic.check_stmt table'' stmt); *)
        (add_func_declarations table' fs)
    end

let handle_semantic_error lexbuf =
  begin
    let pos = lexbuf.lex_start_p in
    fprintf stderr "Semantic error\n";
    fprintf stderr "Near %d:%d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1);
    exit(semantic_error_code);
  end

  let rec eval singleStmt = (match singleStmt with
    | A.SeqStmt (stmt, stmtlist) -> (eval stmt; eval stmtlist)
    | A.SkipStmt(_) ->  ()
    | A.VarDeclStmt(_,sym,exp,_) ->(
       table := Symbol.insert sym exp !table;
      let value = compute (Symbol.lookup sym !table) table in
      Stack.push value stack;
      )
    | A.AssignStmt(lhs, rhs, _) ->(
      table := Symbol.insert (matchLHS lhs) rhs !table;
      let rhs = compute (Symbol.lookup (matchLHS lhs) !table) table in
      Stack.push rhs stack;
      )
    | A.PrintStmt(exp, _) -> (
         Stack.push (compute exp table) stack;
         let result = Stack.pop stack in
            printWrap result;)
    | A.PrintLnStmt(exp, _) ->(
         Stack.push (compute exp table) stack;
         let result = Stack.pop stack in
         printWrap result;
         print_newline ();
       )
    | A.BlockStmt(statment, _) ->
         eval statment)

let () =
  if Array.length Sys.argv <= 1 then
    begin
      print_string usage;
      exit(-1);
    end
  else
    let filename = Sys.argv.(1) in
    let lexbuf = Lexing.from_channel (open_in filename) in
    try
      let (decs, stmt) = Parser.prog Lexer.main lexbuf in
      let () = eval stmt in exit(0);
      (* setup_builtin_functions; *)
      (* let table' = Symbol.new_scope (add_func_declarations table decs) in *)
      (* (\* function declarations *\) *)
      (* ignore(List.map codegen_decl decs); *)
      (* built-in functions *)
      (* the Semantic checking phase *)
      (* ignore(Semantic.check_stmt table' stmt); *)
      (* the main function as entry point*)
      (* let ft = function_type i1_type (Array.make 1 i1_type) in *)
      (* let main_func = declare_function "main" ft the_module in *)
      (* let bb = append_block context "entry" main_func in *)
      (* (position_at_end bb builder); *)
      (* symbol table to keep track of llvm mem location *)
      (* let codegen_table = Symbol.empty in *)
      (* let end_ = build_ret (const_int i1_type 1) builder in *)
      (* position_before end_ builder; *)
      (* the Codegen *)
      (* let _ = codegen_stmt codegen_table stmt in *)
      (* (\* Llvm_analysis.assert_valid_module the_module; *\) *)
      (* let out_filename = (Filename.chop_extension filename) ^ ".ll" in *)
      (* print_module (Filename.basename out_filename) Codegen.the_module; *)

    with
    | A.SyntaxError _ -> handle_syntax_error lexbuf;
    | Semantic.TypeMismatch (expected, actual, pos) ->
      begin
        let (lnum, cnum) = pos_lnum_cnum pos in
        fprintf stderr "Near %d:%d\n" lnum cnum;
        exit(semantic_error_code);
      end
    | Semantic.UnknownIdentifier (err, pos) ->
      begin
        let (lnum, cnum) = pos_lnum_cnum pos in
        fprintf stderr "Near %d:%d\n" lnum cnum;
        fprintf stderr "Unknown Identifier\n";
        exit(semantic_error_code);
      end
    | Semantic.SemanticError (msg, pos) ->
        let (lnum, cnum) = pos_lnum_cnum pos in
        fprintf stderr "%s\n" msg;
        fprintf stderr "Near %d:%d\n" lnum cnum;
        exit(semantic_error_code);
    | Parser.Error ->
       handle_syntax_error lexbuf;
