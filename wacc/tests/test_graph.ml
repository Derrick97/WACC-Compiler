open Il;;
open Graph;;

module RA = Register_allocator;;

let test_insts = [
    LOAD(WORD, "a", ADDR_IMM 2);
    ADD("b", OperReg "a", OperImm 1);
    SUB("c", OperReg "c", OperReg "b");
    ADD("a", OperReg "b", OperImm 2);
    RET("c")
  ]

let test_insts2 = [
    LOAD(WORD, "t1", ADDR_IMM 2);
    ADD("t2", OperReg "t1", OperImm 1);
    SUB("t3", OperReg "t1", OperReg "t2");
    ADD("t4", OperReg "t2", OperImm 2);
    RET("t1")
  ]


let () =
  let insts = test_insts2 in
  let g, ins, outs = Liveness.build insts in
  let igraph = Liveness.build_interference insts outs in
  (* Liveness.show_interference igraph; *)
  let igraph = RA.allocate insts igraph in
  RA.ColoredIGraph.iter_vertex (fun v -> begin
        print_endline (Printf.sprintf "%s: %d" v (RA.ColoredIGraph.Mark.get v));
        ()
      end) igraph;
  ()
