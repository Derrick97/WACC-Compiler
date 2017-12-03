module O = OUnit2;;
open OUnit2;;
open Il;;

let inst_list1 = [PUSH ["r1"]; MOV ("r0", (OperImm(5))); SUB("r5", (OperReg("r5")), (OperReg("r0"))); POP ["r1"]]
let expect_inst1 = [PUSH ["r1"]; SUB ("r5", (OperReg("r5")), (OperImm(5))); POP ["r1"]]

let inst_list2 = [PUSH ["r1"]; MOV ("r5", (OperImm(5))); SUB("r5", (OperReg("r5")), (OperReg("r1"))); POP ["r1"]]
let expect_inst2 = [PUSH ["r1"]; MOV ("r5", (OperImm(5))); SUB("r5", (OperReg("r5")), (OperReg("r1"))); POP ["r1"]]

let inst_list3 = [PUSH ["r1"]; MOV ("r0", (OperReg("r5"))); MOV("r5", (OperReg("r0"))); POP ["r1"]]
let expect_inst3 = [PUSH ["r1"]; MOV ("r0", (OperReg("r5"))); POP ["r1"]]

let inst_list4 = [LOAD (BYTE, "r3",ADDR_INDIRECT("r7",3)); MOV ("r4", OperReg("r3")); MOV ("r3", OperReg("r4")); STORE(WORD, "r7",ADDR_INDIRECT("r3",3))]
let expect_inst4 = [LOAD (BYTE, "r3",ADDR_INDIRECT("r7",3)); MOV ("r4", OperReg("r3")); STORE(WORD, "r7",ADDR_INDIRECT("r3",3))]

let inst_list5 = [LOAD (BYTE, "r3",ADDR_INDIRECT("r7",3)); MOV ("r4", OperReg("r4")); MOV ("r3", OperReg("r4")); STORE(WORD, "r7",ADDR_INDIRECT("r3",3))]
let expect_inst5 = [LOAD (BYTE, "r3",ADDR_INDIRECT("r7",3)); MOV ("r3", OperReg("r4")); STORE(WORD, "r7",ADDR_INDIRECT("r3",3))]



let test1 test_ctxt = assert_equal expect_inst1 (Optimize.peephole_optimize inst_list1)
let test2 test_ctxt = assert_equal expect_inst2 (Optimize.peephole_optimize inst_list2)

let () = print_string "Pattern 1: MOV then ADD/SUB/MUL/DIV\n"
let testsuite = "Pattern 1:testsuite:">:::["test1">:: test1; "test2">:: test2]
let () = run_test_tt_main testsuite

let test3 test_ctxt = assert_equal expect_inst3 (Optimize.peephole_optimize inst_list3)
let test4 test_ctxt = assert_equal expect_inst4 (Optimize.peephole_optimize inst_list4)

let () = print_string "Pattern 2: MOV a b, MOV b a"
let testsuite2 = "Pattern 2:testsuite:">:::["test3">:: test3; "test4">:: test4]
let () = run_test_tt_main testsuite2
