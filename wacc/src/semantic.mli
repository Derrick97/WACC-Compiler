module A = Ast
module S = Symbol

exception SemanticError of string * A.pos
exception TypeMismatch of A.ty * A.ty * A.pos
exception UnknownIdentifier of A.symbol * A.pos
exception UnexpectedError of string * A.pos

type enventry
type env = enventry Symbol.table

val baseenv: env
val check_function_call : env -> S.symbol -> A.exp list -> A.pos -> env
val check_function_decls : A.function_dec list -> unit
val check_exp : env -> A.exp -> env
val check_stmt : env -> A.stmt -> env
val check_int_overflow : int -> int
val add_function_declarations: env -> A.function_dec list -> env
