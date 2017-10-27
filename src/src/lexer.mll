(* lexer for the WACC language *)
{
open Parser
open Lexing

let escape_char c =
  Char.chr (match c with
      | '0' -> 0
      | 'b' -> 0x08
      | 't' -> 0x09
      | 'n' -> 0x0a
      | 'f' -> 0x0c
      | 'r' -> 0x0d
      | '\"' -> 0x22
      | '\'' -> 0x27
      | '\\' -> 0x5c
      | _ -> raise (Invalid_argument "Not an escape char"))
}

let ws    = [' ' '\t']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let newline = '\r' | '\n' | "\r\n"
let ident = ('_'|alpha)('_'| alpha | digit )*
let int_literal = digit+
let escaped_chars = ['0' 'b' 't' 'n' 'f' 'r' '\"' '\'' '\\' ]


rule main = parse
  | ws            { main lexbuf }
  | '\n'          { new_line lexbuf; main lexbuf }
  | '!'           { BANG }
  | '+'           { PLUS }
  | '-'           { MINUS }
  | '*'           { TIMES }
  | '/'           { DIV }
  | '%'           { MOD }
  | '='           { EQ }
  | '\'' ( ('\\'escaped_chars) | [^ '\\' '\'' '\"'] ) '\'' {
      let matched = Lexing.lexeme lexbuf in
      if matched.[1] = '\\' then CHAR (escape_char matched.[2])
      else CHAR (matched.[1])
    }
  | '\"' ( [^ '\\' '\'' '\"'] | ('\\'escaped_chars) )* '\"'
    {
      (* The string parsing is horribly written
         with imperative programming *)
      let str = Lexing.lexeme lexbuf in
      let store_string = Bytes.create (String.length str) in
      let idx = ref 0 in        (* index through the store_string *)
      let to_escape = ref false in
      let i = ref 1 in
      while (!i < (String.length str) - 1) do
        if !to_escape then
           (Bytes.set store_string !idx (escape_char str.[!i]);
            idx := !idx + 1;
            to_escape := false;
            i := !i+1)
        else
        if (str.[!i] = '\\')
        then
          (to_escape := true;
           i := !i + 1;)
        else (Bytes.set store_string !idx str.[!i];
              idx := !idx + 1;
              i:=!i + 1;)
      done;
      STRING (Bytes.to_string store_string)
    }
  | "=="          { EEQ }
  | "!="          { NE }
  | "<="          { LE }
  | ">="          { GE }
  | "&&"          { AND }
  | "||"          { OR }
  | '>'           { GT }
  | '<'           { LT }
  | '#'           { comment lexbuf } (* handle single-line comments *)
  | '('           { LPAREN }
  | ')'           { RPAREN }
  | '['           { LBRACKET }
  | ']'           { RBRACKET }
  | ';'           { SEMICOLON }
  | ','           { COMMA }
  | "null"        { NULL }
  | "begin"       { BEGIN }
  | "end"         { END }
  | "true"        { TRUE }
  | "false"       { FALSE }
  | "while"       { WHILE }
  | "do"          { DO }
  | "done"        { DONE }
  | "null"        { NULL }
  | "read"        { READ }
  | "skip"        { SKIP }
  | "exit"        { EXIT }
  | "free"        { FREE }
  | "len"         { LEN }
  | "ord"         { ORD }
  | "chr"         { CHR }
  | "int"         { INTT }
  | "bool"        { BOOLT }
  | "if"          { IF }
  | "fi"          { FI }
  | "then"        { THEN }
  | "fst"         { FST }
  | "snd"         { SND }
  | "else"        { ELSE }
  | "return"      { RETURN }
  | "newpair"     { NEWPAIR }
  | "pair"        { PAIR }
  | "call"        { CALL }
  | "is"          { IS }
  | "char"        { CHART }
  | "string"      { STRINGT }
  | "println"     { PRINTLN }
  | "print"       { PRINT }
  | int_literal   { let num = int_of_string (Lexing.lexeme lexbuf) in
                    INT(num) }
  | ident         { ID(Lexing.lexeme lexbuf) }
  | eof           { EOF }
  | _             { raise Parser.Error }
and comment = parse
  | '\n'                   { new_line lexbuf; main lexbuf }
  | _                      { comment lexbuf }

(* Local Variables: *)
(* eval: (merlin-mode -1) *)
(* End: *)
