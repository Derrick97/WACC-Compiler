
(* The type of tokens. *)

type token = 
  | UID of (string Positions.located)
  | TYPE
  | TOKEN
  | START
  | STAR
  | RPAREN
  | RIGHT
  | QUESTION
  | PUBLIC
  | PREC
  | PLUS
  | PERCENTPERCENT of (Stretch.t Lazy.t)
  | PERCENTATTRIBUTE
  | PARAMETER
  | ON_ERROR_REDUCE
  | OCAMLTYPE of (Stretch.ocamltype)
  | NONASSOC
  | LPAREN
  | LID of (string Positions.located)
  | LEFT
  | INLINE
  | HEADER of (Stretch.t)
  | GRAMMARATTRIBUTE of (Syntax.attribute)
  | EQUAL
  | EOF
  | COMMA
  | COLON
  | BAR
  | ATTRIBUTE of (Syntax.attribute)
  | ACTION of (Syntax.identifier option array -> Action.t)

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val grammar: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Syntax.partial_grammar)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val grammar: Lexing.position -> (Syntax.partial_grammar) MenhirInterpreter.checkpoint
  
end
