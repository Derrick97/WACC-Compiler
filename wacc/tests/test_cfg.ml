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

let () =
  ignore (Liveness.build insts);
