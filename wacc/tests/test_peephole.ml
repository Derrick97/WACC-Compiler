module O = OUnit2;;
open OUnit2;;
open Il;;

let string_of_insts is = String.concat "\n" (List.map Il.show_il is)

let assert_insts_equal expect actual = assert_equal
    ~printer:string_of_insts expect actual


let inst_list1 = [PUSH ["r1"];
                  MOV ("r0", (OperImm(5)));
                  SUB("r5", (OperReg("r5")), (OperReg("r0")));
                  POP ["r1"]]

let expect_inst1 = [PUSH ["r1"];
                    SUB ("r5", (OperReg("r5")), (OperImm(5)));
                    POP ["r1"]]

let inst_list2 = [PUSH ["r1"];
                  MOV ("r5", (OperImm(5)));
                  SUB("r5", (OperReg("r5")), (OperReg("r1")));
                  POP ["r1"]]

let expect_inst2 = [PUSH ["r1"]; MOV ("r5", (OperImm(5)));
                    SUB("r5", (OperReg("r5")), (OperReg("r1")));
                    POP ["r1"]]

let test1 _ = assert_insts_equal expect_inst1
    (Optimize.peephole_optimize inst_list1)
let test2 _ = assert_insts_equal expect_inst1
    (Optimize.peephole_optimize inst_list2)

let testsuite = "MOV then ADD/SUB/MUL/DIV:"
                >:::["test1">:: test1; "test2">:: test2]

let inst_list3 = [PUSH ["r1"];
                  MOV ("r0", (OperReg("r5")));
                  MOV("r5", (OperReg("r0")));
                  POP ["r1"]]

let expect_inst3 = [PUSH ["r1"];
                    MOV ("r0", (OperReg("r5")));
                    POP ["r1"]]

let inst_list4 = [LOAD (BYTE, "r3", ADDR_INDIRECT("r7",3));
                  MOV ("r4", OperReg("r3"));
                  MOV ("r3", OperReg("r4"));
                  STORE(WORD, "r7",ADDR_INDIRECT("r3",3))]

let expect_inst4 = [LOAD (BYTE, "r3",ADDR_INDIRECT("r7",3));
                    MOV ("r4", OperReg("r3"));
                    STORE(WORD, "r7",ADDR_INDIRECT("r3",3))]

let test3 _ = assert_insts_equal expect_inst3 (Optimize.peephole_optimize inst_list3)
let test4 _ = assert_insts_equal expect_inst4 (Optimize.peephole_optimize inst_list4)

let testsuite2 = "Pattern 2:">:::
                 ["test3">::test3;
                  "test4">:: test4]

let suite = "Peephole">:::
            [testsuite;
             testsuite2]
