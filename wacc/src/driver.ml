open Lexing
open Printf
open Stack
open Interpreter

module A = Ast

let usage = "The WACC compiler\nUsage:\n"
let syntax_error_code = 100
let semantic_error_code = 200

let handle_syntax_error lexbuf =
  let pos = lexbuf.lex_start_p in
    fprintf stderr "Syntax error\n";
    fprintf stderr "Near token \"%s\"\n" (Lexing.lexeme lexbuf);
    fprintf stderr "Near %d:%d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1);
    exit(syntax_error_code)

let parse lexbuf = Parser.prog Lexer.main lexbuf

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
      let (decs, stmt) = parse lexbuf in
      Semantic.check_prog (decs, stmt);
      let table = Semantic.baseenv in
      let table' = Symbol.new_scope (Semantic.add_function_declarations table decs) in
      TranslateSyntax.translate_prog (decs, stmt) table'
      (* TODO backend code generation *)
      (* let stmts, _ = Semantic.translate table' frame stmt in
       * let out_filename = (Filename.chop_extension (Filename.basename filename)) ^ ".s" in
       * let out = open_out out_filename in
       * Translate.print_insts out frame stmts;
       * close_out out; *)
      (* ignore(Sys.command ("cat wacclib.s >> " ^ out_filename)); *)
    with
    | A.SyntaxError _ | Parser.Error _ -> handle_syntax_error lexbuf
