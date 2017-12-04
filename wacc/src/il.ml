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
  | CALL  of label
  | COMP  of temp * temp
  | CBR   of temp * label * label
  | RET   of temp
  | LABEL of label  [@@deriving show]

let print_operand operand =
  match operand with
  | OperImm(num) -> print_int num; print_string " "
  | OperReg(reg) -> print_string reg; print_string " "

let inst_to_print il =
  match il with
  | PUSH [temp] -> print_string ("push" ^ temp)
  | POP [temp] -> print_string ("pop" ^ temp)
  | ADD (temp, op1, op2) -> print_string ("add " ^ temp ^ " "); print_operand op1; print_operand op2
  | SUB (temp, op1, op2) -> print_string ("sub " ^ temp ^ " "); print_operand op1; print_operand op2
  | MUL (temp, op1, op2) -> print_string ("mul " ^ temp ^ " "); print_operand op1; print_operand op2
  | MOV _ -> print_string "mov"
  | _ -> print_string "TODO"


let push dsts = PUSH (dsts)
let pop dsts = POP (dsts)
let mov dst op = MOV (dst, op)
let add dst op1 op2 = ADD (dst, op1, op2)
let sub dst op1 op2 = SUB (dst, op1, op2)
let div dst op1 op2 = DIV (dst, op1, op2)
let eor dst op1 op2 = EOR (dst, op1, op2)
let mul dst op1 op2 = MUL (dst, op1, op2)
let and_ dst op1 op2 = AND (dst, op1, op2)
let or_ dst op1 op2 = ORR (dst, op1, op2)
let cmp cond dst op1 op2 = CMP (cond, dst, op1, op2)
let load sz dst addr = LOAD (sz, dst, addr)
let store sz dst addr = STORE (sz, dst, addr)
let jump l = JUMP l
let call l = CALL l
let comp t0 t1 = COMP (t0, t1)
let cbr t th el = CBR (t, th, el)
let ret t = RET t
let label l = (LABEL l)


type emitter = {
  mutable emit_counter: int;
  mutable emit_code: il array;
}

let new_emitter () =
  {
    emit_counter = 0;
    emit_code = Array.make 4 NOOP;
  }

let append (emitter: emitter) (i:il): unit = begin
  let size = Array.length emitter.emit_code in
  (if (emitter.emit_counter >= size)
  then
    let old = emitter.emit_code in
    let n = Array.make (2*size) NOOP in
    Array.blit old 0 n 0 size;
    ignore (emitter.emit_code <- n)
  else
    ());
  emitter.emit_code.(emitter.emit_counter) <- i;
  emitter.emit_counter <- emitter.emit_counter + 1;
end
