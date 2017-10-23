module A = Ast
module S = Symbol
open Printf

module StackFrame = struct
  type t =
    | Array of int list
    | Int of int
    | String of string
    | Bool of bool
    | Pair of t * t
    | Char of char
  type scope_t = t S.table
  let create: scope_t = S.empty
  let create_subframe frame = ()
  let add table key value =
    begin
      S.insert table key value
    end
end

let code = ref "foo"
let env: (StackFrame.scope_t ref) = ref StackFrame.create
let translate_binop op = match op with
  | A.PlusOp -> code := "+\n"
  | _ -> ()

let rec translate_exp exp = match exp with
  | A.BinOpExp (left, op, right, _) ->
     begin
       let left_val = translate_exp left and
           right_val = translate_exp right in
       ()
     end

let translate_stmt env stmt = match stmt with
  | A.SkipStmt (_) -> code := "skip\n";
  | _ -> ()

let () =
  printf "before %s\n" !code;
  translate_binop A.PlusOp;
  printf "after %s" !code;
