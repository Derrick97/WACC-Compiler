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

let () =
  run_test_tt_main suite
;;
