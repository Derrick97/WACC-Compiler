module IL = Il;;
open Graph;;

module CFG = struct
  (** Control-flow graph *)
  module V_ = struct
    type t = IL.il
    let equal a b = (String.compare (IL.show_il a) (IL.show_il b) == 0)
    let compare a b = (String.compare (IL.show_il a) (IL.show_il b))
    let hash i = Hashtbl.hash (IL.show_il i)
  end

  include Graph.Imperative.Digraph.Concrete(V_)

end

module InOutSet = Set.Make(String);;

module Flow = struct
  (* dataflow analysis *)
  type graph = CFG.t
  type node = CFG.V.label
  type set = InOutSet.t
  type t = {
    live_cfg: graph;
    uses: (node, InOutSet.t) Hashtbl.t;
    defs: (node, InOutSet.t) Hashtbl.t
  }

  let def (i:IL.il) =
    let open IL in
    match i with
    | ADD (OperReg dst, op1, op2) -> InOutSet.of_list [dst]
    | SUB (OperReg dst, op1, op2) -> InOutSet.of_list [dst]
    | LOAD (OperReg dst, _) -> InOutSet.of_list [dst]
    | RET (OperReg r) -> InOutSet.of_list []
    | _ -> invalid_arg "TODO"

  let use (i:IL.il) =
    let open IL in
    match i with
    | ADD (dst, OperReg op1, OperReg op2) | SUB (dst, OperReg op1,  OperReg op2) -> InOutSet.of_list [op1; op2]
    | ADD (dst, OperReg op1, _) | SUB (dst, OperReg op1, _) -> InOutSet.of_list [op1]
    | ADD (dst, _, OperReg op2) | SUB (dst, _, OperReg op2) -> InOutSet.of_list [op2]
    | RET (OperReg r) -> InOutSet.of_list [r]
    | _ -> InOutSet.empty

  let build (instrs: IL.il list): t = begin
    (* solve the dataflow equations
       See modern compiler implementation in ML, page 214 Algo 10.4 for pseudo-code.
    *)
    (* first we build the control flow graph *)
    let cfg = CFG.create() in
    let print_set s = begin
      print_endline ("{" ^ (String.concat "," (InOutSet.elements s)) ^ "}")
    end in
    print_endline "initializing def and use sets";
    let (uses: (CFG.V.label, InOutSet.t) Hashtbl.t) = Hashtbl.create 0 in
    let (defs: (CFG.V.label, InOutSet.t) Hashtbl.t) = Hashtbl.create 0 in
    List.iter (fun i ->
        Hashtbl.add uses i (use i);
        Hashtbl.add defs i (def i)) instrs;
    let ii = ref 0 in
    List.iter (fun i ->
        (* CFG.add_vertex cfg (CFG.V.create i); *)
        if !ii > 0 then
          (let prev = List.nth instrs (!ii-1) in
           CFG.add_edge cfg prev i;
           ())
        else ();
        ii := !ii + 1;
        (* print_int (!ii); print_newline(); *)
      ) instrs;

    print_endline "defs";
    Hashtbl.iter (fun k v -> (
          print_set v)) defs;
    print_endline "uses";
    Hashtbl.iter (fun _ v -> print_set v) uses;
    print_endline "initializing dataflow solver";
    (* initialize *)

    let (in_: (CFG.V.t, InOutSet.t) Hashtbl.t) = Hashtbl.create 0 in
    let (out: (CFG.V.t, InOutSet.t) Hashtbl.t) = Hashtbl.create 0 in
    CFG.iter_vertex (fun v ->
        Hashtbl.add in_ v (InOutSet.empty);
        Hashtbl.add out v (InOutSet.empty);
      ) cfg;
    (* now iteratively solve *)
    let i = ref 0 in
    let rec loop () = begin
      let terminate = ref true in
      let open Printf in
      printf "beginning iteration %d\n" !i;
      (* printf "uses and defs -------\n";
       * CFG.iter_vertex (fun v ->
       *     print_endline "-----------";
       *     print_endline (IL.show_il v);
       *     print_set (Hashtbl.find uses v);
       *     print_set (Hashtbl.find defs v);
       *     print_endline "-----------";
       *   ) cfg;
       * printf "end uses and defs -------\n"; *)
      CFG.iter_vertex (fun v -> begin
            let in'  = Hashtbl.find in_ v in (* in'[n] <- in[n] *)
            let out' = Hashtbl.find out v in (* out'[n] <- out[n] *)
            let new_in = InOutSet.union
                (Hashtbl.find uses v)
                (InOutSet.diff
                   (Hashtbl.find out v)
                   (Hashtbl.find defs v)) in (* in[n] = use[n] U (out[n] - def[n]) *)
            let new_out = List.fold_left
                (fun a b -> InOutSet.union a (Hashtbl.find in_ b))
                (InOutSet.empty) ((CFG.succ cfg v)) in (* out[n] = U_{s \in succ(n)} in[s] *)
            (* print_endline "------";
             * print_set in';
             * print_set out';
             * print_set new_in;
             * print_set new_out;
             * print_endline "------"; *)
            if not (InOutSet.equal in' new_in) &&
               (InOutSet.equal out' new_out)  then
              terminate := false else ();
            Hashtbl.add in_ v new_in;
            Hashtbl.add out v new_out;
            i := !i + 1;
          end) cfg;
      CFG.iter_vertex (fun v ->
          print_endline "-----------";
          print_endline (IL.show_il v);
          print_endline "in";
          print_set (Hashtbl.find in_ v);
          print_endline "out";
          print_set (Hashtbl.find out v);
          print_endline "-----------";
        ) cfg;
      if not !terminate then loop() else ()
    end in
    loop ();
    {live_cfg=cfg; uses=uses; defs=defs}
  end
end



let () =
  let open IL in
  let test_insts = [
    LOAD(OperReg "a", ADDR_IMM 2);
    ADD(OperReg "b", OperReg "a", OperImm 1);
    SUB(OperReg "c", OperReg "c", OperReg "b");
    ADD(OperReg "a", OperReg "b", OperImm 2);
    SUB(OperReg "d", OperReg "c", OperReg "a");
    RET(OperReg "c")
  ] in
  let g = Flow.build test_insts in
  ()

(* ocamlbuild -use-ocamlfind -package ocamlgraph -package unix -package ppx_deriving.std src/test_graph.byte *)
