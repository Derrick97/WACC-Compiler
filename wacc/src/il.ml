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
  | ADDR_IMM of int
and opcode =
  | Op_add
and il =
  | ADD   of temp * temp * temp
  | SUB   of operand * operand * operand
  | DIV   of operand * operand * operand
  | MUL   of operand * operand * operand
  | AND   of operand * operand * operand
  | ORR   of operand * operand * operand
  | MOV   of operand * operand
  | CMP   of operand * operand
  | LOAD  of operand * addr
  | STORE of operand * addr
  | POP   of operand
  | PUSH  of operand
  | BL    of label
  | B     of label
  | RET   of operand
  | LABEL of label  [@@deriving show]
