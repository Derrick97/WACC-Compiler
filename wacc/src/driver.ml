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

(* helper function to get the linum and colnum of the position in a tuple *)
let pos_lnum_cnum pos =
  let lnum = pos.pos_lnum in
  let cnum = (pos.pos_cnum - pos.pos_bol + 1) in
  (lnum, cnum)

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
      ignore(Semantic.check_function_decls decs);
      ignore(Prettyprint.prettyprint_stmt stmt);
      let table = Symbol.empty in
      let table' = Symbol.new_scope (add_func_declarations table decs) in
      ignore(Semantic.check_stmt table' stmt); ()
    with
    | A.SyntaxError _ -> handle_syntax_error lexbuf;
    | Semantic.TypeMismatch (expected, actual, pos) ->
      begin
        let (lnum, cnum) = pos_lnum_cnum pos in
        fprintf stderr "TypeMismatch Near %d:%d\n" lnum cnum;
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
