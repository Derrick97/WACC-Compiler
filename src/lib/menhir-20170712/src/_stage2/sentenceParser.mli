
(* The type of tokens. *)

type token = 
  | TERMINAL of (Grammar.Terminal.t * Lexing.position * Lexing.position)
  | NONTERMINAL of (Grammar.Nonterminal.t * Lexing.position * Lexing.position)
  | EOL
  | EOF
  | COMMENT of (string)
  | COLON

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val optional_sentence: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (SentenceParserAux.sentence option)

val entry: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (SentenceParserAux.located_sentence SentenceParserAux.or_comment list)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val optional_sentence: Lexing.position -> (SentenceParserAux.sentence option) MenhirInterpreter.checkpoint
  
  val entry: Lexing.position -> (SentenceParserAux.located_sentence SentenceParserAux.or_comment list) MenhirInterpreter.checkpoint
  
end
