module IL = Il

type color = int

module ColoredIGraph = struct
  include Liveness.IGraph

  let colorMap: (V.t, int) Hashtbl.t = Hashtbl.create 0

  module Mark = struct
    let get (v:V.t): color = Hashtbl.find colorMap v
    let set (v:V.t) (color: int): unit = begin
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
    (insts: IL.il list)
    (igraph: Liveness.IGraph.t): ColoredIGraph.t =
  let igraph = ColoredIGraph.from_igraph igraph in
  ColoredIGraph.iter_vertex (fun v -> ColoredIGraph.Mark.set v 0) igraph;
  C.coloring igraph 32;
  igraph
