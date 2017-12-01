open OUnit2;;
open Il;;

let suite = "Test Main">:::
            [Test_peephole.suite]

let () = run_test_tt_main suite
