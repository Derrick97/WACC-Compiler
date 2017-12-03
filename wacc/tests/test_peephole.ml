(*
  OUnit documentation can be found here
  http://ounit.forge.ocamlcore.org/api-ounit/index.html *)

module O = OUnit2;;
open OUnit2;;
open Il;;

show
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

let inst_list5 = [LOAD (BYTE, "r3",ADDR_INDIRECT("r7",3));
                  MOV ("r4", OperReg("r4"));
                  MOV ("r3", OperReg("r4"));
                  STORE(WORD, "r7",ADDR_INDIRECT("r3",3))]
let expect_inst5 = [LOAD (BYTE, "r3",ADDR_INDIRECT("r7",3));
                   MOV ("r3", OperReg("r4"));
                   STORE(WORD, "r7",ADDR_INDIRECT("r3",3))]

let testsuite1 = "MOV then ADD/SUB/MUL/DIV:">:::
    List.mapi (fun i (expected, actual) -> begin
        let title = (string_of_int i) in
        title >:: (fun _ ->
          assert_insts_equal expected (Optimize.peephole_optimize actual))
      end) [(expect_inst1, inst_list1);
            (expect_inst2, inst_list2);]

let testsuite2 = "MOV a b then MOV b a:">:::
    List.mapi (fun i (expected, actual) -> begin
        let title = (string_of_int i) in
        title >:: (fun _ ->
          assert_insts_equal expected (Optimize.peephole_optimize actual))
        end) [(expect_inst3, inst_list3);
              (expect_inst4, inst_list4);]

let testsuite3 = "MOV a a">:::
    List.mapi (fun i (expected, actual) -> begin
        let title = (string_of_int i) in
        title >:: (fun _ ->
          assert_insts_equal expected (Optimize.peephole_optimize actual))
        end) [(expect_inst5, inst_list5)]

let suite = "Peephole Optimization">:::
            [testsuite1; testsuite2; testsuite3]
