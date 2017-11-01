module S = Symbol;;
module A = Ast;;
module T = Tree;;

type frag = unit
type size = int

and  access =
  | InFrame of int * size
  | InReg of int * size

and exp =
  | Imm of int * size
  | InAccess of access
  | Label of string

type frame = {
  mutable frame_counter: int;
  mutable frame_locals: access array;
}

let new_frame frame = {
    frame_counter = 0;
    frame_locals = [| |];
}

let trans_unop   (op: A.unop) (exp: exp) = match op with
  | A.NotOp -> failwith "TODO"
  | _ -> failwith "TODO trans_unop"

let trans_binop  (op: A.binop) (lhs: exp) (rhs: exp) = match (lhs, rhs) with
  | _ -> assert false

let trans_lit    (l: A.literal) = match l with
  | _ -> assert false

let temp_counter = ref 0

let new_temp (): string =
  let i = !temp_counter in
  temp_counter := i + 1;
  "t" ^ (string_of_int i)

let new_label (): string = "l"
let new_namedlabel name = name

let trans_ifelse (cond: exp) (t: exp) (f: exp) = begin
  let true_l = new_namedlabel "if_then" in
  let false_l = new_namedlabel "if_else" in
  let end_l = new_namedlabel "if_end" in
  failwith "TODO"
end

let regFP: Temp.temp = 0

let trans_var    (var: access): exp = match var with
  | _ -> assert false

let trans_array  (var: access) (indices: exp list) = failwith "TODO"
let trans_assign (lv: exp) (rv: exp) = begin
  failwith "TODO"
end

let trans_while  (cond: exp) (body: exp) = begin
  let while_cond_l = new_namedlabel "while_cond" in
  let while_end_l = new_namedlabel "while_done" in
  failwith "TODO"
end

let trans_seq    (first: exp) (follow: exp) = begin
  failwith "TODO"
end

let trans_call   (fname: string) (args: exp list) = begin
  let fname_label = new_namedlabel fname in
  failwith "TODO trans_call"
end

let allocate_local (frame: frame) (size: size) =
  let i = frame.frame_counter in
  frame.frame_counter <- i + 1;
  InFrame (i, size)

let trans_noop: exp = Tree.Const (0, Ast.IntTy)

let access_of_exp (exp: exp): access = failwith "TODO"

let function_prologue (frame: frame) (args: access list): unit = failwith "TODO"
let function_epilogue (frame: frame) = failwith "TODO"

let rec codegen_exp  (exp: Tree.exp): Temp.temp = failwith "TODO"
and     codegen_stmt (stmt: Tree.stmt): unit = ()
