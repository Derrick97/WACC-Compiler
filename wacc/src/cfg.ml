module IL = Il

(** Control-flow graph *)
module V_ = struct
  type t = IL.il * int
  let equal a b = (snd a) = (snd b)
  let compare a b = (Pervasives.compare (snd a) (snd b))
  let hash i = Hashtbl.hash (snd i)
end

include Graph.Imperative.Digraph.Concrete(V_)

(** build a Control-Flow Graph (CFG) from a list of instructions *)
let build_cfg (instrs: (IL.il * int) list): t = begin
  let g = create() in
  let open IL in
  (* add all instructions to the graph,
     add edges for consecutive instructions
  *)
  let jump_table = Hashtbl.create 0 in
  List.iteri (fun i inst ->
      add_vertex g inst;
      (match inst with
      | LABEL l, j -> Hashtbl.add jump_table l j
      | _, _ -> ());
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
    | JUMP  label, _ -> begin
        add_edge g n ((LABEL label), Hashtbl.find jump_table label)
      end
    | CBR  (_, l0, l1), _ -> begin
        add_edge g n ((LABEL l0), Hashtbl.find jump_table l0);
        add_edge g n ((LABEL l1), Hashtbl.find jump_table l1);
      end
    | _ -> ()
  end in
  iter_vertex resolve_jumps g;
  g
end
