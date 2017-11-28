(** An Intermediate language similar to ILOC
   The goal of the designing this IR is two fold
   1) we desugar as much information as possible from the frontend.
   2) we provide good representation for data-flow analysis,
    optimisation and code generation.

    One design requirement is to make control flow explicit in the IR.
    Hence, for branch instructions, all branches must be explicitly present in
    the instructions

    We use a small subset of the ILOC IR, which is introduced
    in the book Engineering a Compiler.
*)

type label = string [@@deriving show]
type temp = string [@@deriving show]
and size =
  | WORD
  | BYTE [@@deriving show]
type operand =
  | OperReg of temp
  | OperImm of int [@@deriving show]
and cond =  GT | GE | LT | LE | EQ | NE | VS [@@deriving show]
and addr =
  | ADDR_LABEL of label
  | ADDR_INDIRECT of temp * int
and il =
  | NOOP
  | PUSH  of temp list
  | POP   of temp list
  | MOV   of temp * operand
  | ADD   of temp * operand * operand
  | EOR   of temp * operand * operand
  | SUB   of temp * operand * operand
  | DIV   of temp * operand * operand
  | MUL   of temp * operand * operand
  | AND   of temp * operand * operand
  | ORR   of temp * operand * operand
  | CMP   of cond * temp * operand * operand
  | LOAD  of size * temp * addr
  | STORE of size * temp * addr
  | JUMP  of label
  | COMP  of temp * temp
  | CBR   of temp * label * label
  | RET   of temp
  | LABEL of label  [@@deriving show]
