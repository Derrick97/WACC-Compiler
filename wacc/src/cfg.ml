module IL = Il

(** Control-flow graph *)
module V_ = struct
  type t = IL.il
  let equal a b = (String.compare (IL.show_il a) (IL.show_il b) == 0)
  let compare a b = (String.compare (IL.show_il a) (IL.show_il b))
  let hash i = Hashtbl.hash (IL.show_il i)
end

include Graph.Imperative.Digraph.Concrete(V_)

(** build a Control-Flow Graph (CFG) from a list of instructions *)
let build_cfg (instrs: IL.il list): t = begin
  let g = create() in
  let open IL in
  (* add all instructions to the graph,
     add edges for consecutive instructions
  *)
  List.iteri (fun i inst ->
      add_vertex g inst;
      if (i < (List.length instrs) - 1) then
        let next_inst = List.nth instrs (i+1) in
        (* print_int(i); print_string "-"; print_int (i+1);
         * print_newline(); *)
        add_edge g inst next_inst
      else
        ()
    ) instrs;

  (* Resolve jumps in the control flow *)
  (* NOTE: labels are valid instructions in IL *)
  let resolve_jumps n = begin
    match n with
    | JUMP  label -> begin
        add_edge g (LABEL label) n
      end
    | CBR  (_, l0, l1) -> begin
        add_edge g (LABEL l0) n;
        add_edge g (LABEL l1) n;
      end
    | _ -> ()
  end in
  iter_vertex resolve_jumps g;
  g
end
