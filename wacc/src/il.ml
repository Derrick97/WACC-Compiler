type label
type temp
and size =
  | WORD
  | BYTE
type operand =
  | OperReg of int
  | OperImm of int * size
and cond =  GT | GE | LT | LE | EQ | NE | VS
and addr =
  | ADDR_LABEL of label
  | ADDR_INDIRECT of temp * int
and il' =
  | ADD   of operand * operand * operand
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
  | LABEL of label
and il = il' * cond
