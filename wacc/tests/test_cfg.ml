open OUnit2;;

let unity x = x;;
let funix ()= 0;;
let fgeneric () = failwith "Not implemented";;

let test1 test_ctxt = assert_equal "x" (unity "x");;

let test2 test_ctxt = assert_equal 100 (unity 100);;
let test3 test_ctxt = fgeneric ();;

(* Name the test cases and group them together *)
let suite =
"suite">:::
 ["test1">:: test1;
  "test2">:: test2;
  "test3">:: test3;
 ]
;;

open Il;;

let insts =
  [MOV (("a"), (OperImm 0)), 0;
   LABEL "if.cond", 1;
   ADD (("b"), (OperReg "a"), (OperImm 0)), 2;
   ADD (("c"), (OperReg "c"), (OperReg "b")), 3;
   ADD (("a"), (OperReg "b"), (OperImm 2)), 4;
   JUMP "if.cond", 5;
   RET ("c"), 6
  ]

let insts2 =
  [MOV (("a"), (OperImm 5)), 1;
   MOV (("c"), (OperImm 1)), 2;
   LABEL "L1", 3;
   ADD (("c"), (OperReg "c"), (OperReg "c")), 4;
   ADD (("c"), (OperReg "c"), (OperReg "b")), 5;
   ADD (("a"), (OperReg "b"), (OperImm 2)), 6;
   JUMP "L1", 7;
   LABEL "L2", 8;
   SUB (("a"), (OperReg "c"), (OperReg "a")), 9;
   MOV ("c", (OperImm 0)), 10
  ]

let () =
  (* ignore (Liveness.build insts); *)
  let reachin, reachout = (Constprop.build_reach insts2) in
  let optimized = Constprop.constant_prop insts2 reachin in
  List.iter (fun (i, pos) -> begin
        print_string (Il.show_il i);
        print_newline();
      end) optimized;
  ()
