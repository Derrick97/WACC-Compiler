module A = Ast
module S = Symbol
open Env;;

exception SemanticError of string * A.pos
exception TypeMismatch of A.ty * A.ty * A.pos
exception UnknownIdentifier of A.symbol * A.pos
exception UnexpectedError of string * A.pos

val baseenv: env
val check_function_call : env -> S.symbol -> A.exp list -> A.pos -> A.ty
val check_function_decls : A.function_dec list -> unit
val check_exp : env -> A.exp -> A.ty
val check_stmt : env -> A.stmt -> env
val translate: env -> Translate.frame -> A.stmt -> Translate.stmt list * env
val add_function_declarations: env -> A.function_dec list -> env
