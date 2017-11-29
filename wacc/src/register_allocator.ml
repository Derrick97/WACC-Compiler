module IL = Il

type color = int

module ColoredIGraph = struct
  include Liveness.IGraph

  let colorMap: (V.t, int) Hashtbl.t = Hashtbl.create 0

  module Mark = struct
    let get (v:V.t): color = Hashtbl.find colorMap v
    let set (v:V.t) (color: color): unit = begin
      (* print_string v; print_string " ";
       * print_int color; print_newline(); *)
      Hashtbl.add colorMap v color
    end
  end

  let from_igraph (g: Liveness.IGraph.t):t = begin
    let g' = create() in
    iter_vertex (fun v -> add_vertex g' v) g;
    iter_edges_e (fun e -> add_edge_e g' e) g;
    g'
    end
end

module C = Graph.Coloring.Mark(ColoredIGraph)

let allocate
    (insts: (IL.il*int) list)
    (igraph: Liveness.IGraph.t): (string, string) Hashtbl.t =
  let igraph = ColoredIGraph.from_igraph igraph in
  let open Arm in
  let module G = ColoredIGraph in
  let builtin_regs = List.concat ([caller_saved_regs; callee_saved_regs; [reg_SP; reg_LR; reg_PC]]) in
  G.iter_vertex (fun n -> ColoredIGraph.Mark.set n 0) igraph;
  List.iteri (fun i r -> ColoredIGraph.Mark.set r (i + 1)) builtin_regs;
  C.coloring igraph (List.length builtin_regs);
  let colormap = Hashtbl.create 0 in
  ColoredIGraph.iter_vertex (fun n -> begin
        let c = ColoredIGraph.Mark.get n in
        if (List.mem n builtin_regs) then
          Hashtbl.add colormap n n
        else
          Hashtbl.add colormap n (List.nth builtin_regs (c-1))
  end) igraph;
  colormap
