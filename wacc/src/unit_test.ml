module O = OUnit2;;
open OUnit2;;
open Il;;

let inst_list2 = [PUSH ["r1"]; MOV ("r0", (OperImm(5))); SUB("r5", (OperReg("r5")), (OperReg("r0"))); POP ["r1"]]

let expect_inst1 = [PUSH ["r1"]; SUB ("r5", (OperReg("r5")), (OperImm(5))); POP ["r1"]]

let test1 test_ctxt = assert_equal expect_inst1 (Optimize.peephole_optimize inst_list2)

let testsuite = "test1">:: test1

let () = run_test_tt_main testsuite
