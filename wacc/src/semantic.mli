module A = Ast
module S = Symbol

exception SemanticError of string * A.pos
exception TypeMismatch of A.ty * A.ty * A.pos
exception UnknownIdentifier of A.symbol * A.pos
exception UnexpectedError of string * A.pos
exception SomeError of string


type enventry = VarEntry of A.ty | FuncEntry of A.ty * A.ty list
type 'a table = 'a S.table
val var_name : A.exp -> A.symbol
val var_type : enventry S.table -> S.symbol -> A.ty
val binop_type : A.binop -> A.ty
val exp_type : enventry S.table -> A.exp -> A.ty
val eq_type : A.ty -> Ast.ty -> bool
val type_mismatch : A.ty -> A.ty -> A.pos -> 'a
val check_function_call : enventry S.table -> S.symbol -> A.exp list -> A.pos -> enventry S.table
val check_function_decls : A.function_dec list -> unit
val check_exp : enventry S.table -> A.exp -> enventry S.table
val check_stmt : enventry S.table -> A.stmt -> enventry S.table
val check_int_overflow : int -> int
val unop_types : (A.unop * A.ty * A.ty) list
val unop_arg_type : A.unop -> A.ty
val unop_ret_type : A.unop -> A.ty
