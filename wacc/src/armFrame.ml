type frame = {
  frame_name: string;
  mutable frame_counter: int;
}

type access = string
type reg = int

let new_frame name = {
  frame_name = name;
  frame_counter = 0;
}

let allocate_local frame = "f"

let reg_SP = 13
let reg_LR = 14
let reg_PC = 15
let registers = [0; 1; 2;
                 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; (* General purpose registers *)
                 13;                              (* SP *)
                 14;                              (* LR *)
                 15;                              (* PC *)
                ]

let argregs = []
let callersaves = []
let calleesaves = []
