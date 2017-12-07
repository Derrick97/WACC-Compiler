(* dataflow analysis *)
module CFG = Cfg;;
module IL = Il;;

module InOutSet = Set.Make(String);;

type il = IL.il;;
type graph = CFG.t
type tempset = InOutSet.t
type t = {
  live_cfg: graph;
  live_uses: (Il.il, InOutSet.t) Hashtbl.t;
  live_defs: (Il.il, InOutSet.t) Hashtbl.t
}

module Temp = struct
  include String
  let equal a b = (String.compare a b = 0)
  let compare = String.compare
  let hash i = Hashtbl.hash i
end

(** compute definitions of variables in instruction *)
let def (i:(IL.il*int)) =
  let open IL in
  let i = fst i in
  let defs = (match i with
      | ADD   (dst, _, _)
      | SUB   (dst, _, _)
      | DIV   (dst, _, _)
      | MUL   (dst, _, _)
      | AND   (dst, _, _)
      | EOR   (dst, _, _)
      | ORR   (dst, _, _)
      | CMP   (_, dst, _, _) -> [dst]
      | LOAD  (_, dst, _) -> [dst]
      | STORE _ -> []
      | MOV (t, _) -> [t]
      | POP ts -> ts
      | PUSH _ | JUMP _ | LTORG | COMP _ | CBR _ | RET _ | LABEL _ | NOOP  | CALL _ -> [])

  in
  InOutSet.of_list defs

(** compute uses of variables in instruction *)
let use (i:(il*int)):tempset =
  let open IL in
  let i = fst i in
  let is_reg = function
    | OperReg _ -> true
    | OperImm _ -> false in
  let get_temp = function
    | OperReg r -> r
    | _ -> invalid_arg "not OperReg" in

  InOutSet.of_list (match i with
      | ADD   (_, op1, op2) | SUB (_, op1, op2)
      | DIV   (_, op1, op2) | MUL   (_, op1, op2)
      | AND   (_, op1, op2) | ORR   (_, op1, op2) | EOR (_, op1, op2)
      | CMP   (_, _, op1, op2) -> List.map get_temp
                                    (List.filter is_reg [op1; op2])
      | COMP  (op1, op2) -> [op1; op2]
      | LOAD  (_, _, ADDR_INDIRECT (base, _)) -> [base]
      | LOAD  (_, _, _) -> []
      | STORE (_, t, (ADDR_INDIRECT (base, _))) -> [t; base]
      | STORE (_, t, _) -> [t]
      | CBR   (t, _ , _) -> [t]
      | RET   t -> [t]
      | JUMP _ | LABEL _ | NOOP  -> []
      | LTORG -> []
      | MOV (_, op1) -> if is_reg op1 then [get_temp op1] else []
      | PUSH ts | POP ts -> ts
      | CALL _ -> []
    )

let string_of_set s =
  ("{" ^ (String.concat "," (InOutSet.elements s)) ^ "}")

let print_set s = (print_endline (string_of_set s))

(** build a livenes graph *)
let build (instrs: (il*int) list) = begin
  (* solve the dataflow equations
     See modern compiler implementation in ML, page 214 Algo 10.4 for pseudo-code.
  *)
    (* first we build the control flow graph *)
  let cfg = CFG.build_cfg instrs in
  let insts_rev = List.rev instrs in
  (* print_endline "initializing def and use sets"; *)
  let (uses: (CFG.V.t, InOutSet.t) Hashtbl.t) = Hashtbl.create 0 in
  let (defs: (CFG.V.t, InOutSet.t) Hashtbl.t) = Hashtbl.create 0 in
  List.iter (fun i ->
      Hashtbl.add uses i (use i);
      Hashtbl.add defs i (def i)) instrs;

  (* print_endline "defs";
   * List.iter (fun ((i, _) as i') -> (
   *       print_endline (IL.show_il i);
   *       print_endline @@ string_of_set (Hashtbl.find defs i');
   *     )) instrs;
   * print_endline "uses";
   * List.iter (fun ((i, _) as i') -> (
   *       print_endline (IL.show_il i);
   *       print_endline @@ string_of_set (Hashtbl.find uses i');
   *     )) instrs; *)
  (* initialize *)
  let nbv = CFG.nb_vertex cfg in
  (* print_int nbv;
   * print_endline "initializing dataflow solver"; *)
  let (in_: (CFG.V.t, InOutSet.t) Hashtbl.t) = Hashtbl.create nbv in
  let (out: (CFG.V.t, InOutSet.t) Hashtbl.t) = Hashtbl.create nbv in
  CFG.iter_vertex (fun n ->
      let (_, i) = n in
      (* print_int(i);
       * print_string " ";
       * print_int(Hashtbl.hash n);
       * print_newline();
       * flush_all(); *)
      Hashtbl.replace in_ n (InOutSet.empty);
      Hashtbl.replace out n (InOutSet.empty);
    ) cfg;
  (* now iteratively solve *)
  let i = ref 0 in
  let rec loop () = begin
    let terminate = ref true in
    i := !i + 1;
    let open Printf in
    (* printf "Iteration %d\n" !i; *)
    flush_all();
    List.iter (fun n -> begin
          let in'  = Hashtbl.find in_ n in (* in'[n] <- in[n] *)
          let out' = Hashtbl.find out n in (* out'[n] <- out[n] *)
          let new_in = InOutSet.union
              (Hashtbl.find uses n)
              (InOutSet.diff
                 (Hashtbl.find out n)
                 (Hashtbl.find defs n)) in (* in[n] = use[n] U (out[n] - def[n]) *)
          let new_out = List.fold_left
              (fun a b -> InOutSet.union a (Hashtbl.find in_ b))
              (InOutSet.empty) ((CFG.succ cfg n)) in (* out[n] = U_{s \in succ(n)} in[s] *)
          (* print_endline ((IL.show_il (fst n)) ^ "------");
           * print_string "old in ";
           * print_set in';
           * print_string "old out ";
           * print_set out';
           * print_string "new in ";
           * print_set new_in;
           * print_string "new out ";
           * print_set new_out;
           * print_endline "------"; *)
          if not((InOutSet.equal in' new_in) && (InOutSet.equal out' new_out)) then
            terminate := false else ();
          Hashtbl.replace in_ n new_in;
          Hashtbl.replace out n new_out;
        end) instrs;
    (* List.iter (fun ((v,_) as v') ->
     *     print_endline (IL.show_il v);
     *     print_string "in  ";
     *     print_endline @@ string_of_set (Hashtbl.find in_ v');
     *     print_string "out ";
     *     print_endline @@ string_of_set (Hashtbl.find out v');
     *     print_endline "-----------";
     *   ) instrs; *)
    if not !terminate then loop () else ()
  end in
  loop ();
  out
end


(* An interference graph has temporaries as nodes and interference between
   two temporaries as edges. It is used for register allocation in later
   passes of the backend.

   See modern compiler implementaion in ML for algorithm
*)

module IGraph = Graph.Imperative.Graph.Concrete(Temp)
type igraph = IGraph.t

(** Build an interference graph *)
let build_interference (insts: (il*int) list) (liveMap: ((il*int), tempset) Hashtbl.t) =
  let igraph = IGraph.create () in
  let open IL in
  let iter_inst inst = begin  (* TODO handle move specially *)
      (match inst with
       | IL.MUL (dst, OperReg t0, OperReg t1),_ -> (
         (if (String.compare dst t0 != 0) then
            IGraph.add_edge igraph dst t0 else ());
         (if (String.compare dst t1 != 0) then
            IGraph.add_edge igraph dst t1 else ());
         ())
         | _ -> ()
      );
      InOutSet.iter (fun d -> begin
              let ts = Hashtbl.find liveMap inst in
              (* Add the vertices *)
              InOutSet.iter (fun t -> if (String.compare d t != 0) (* No edges to self *)
                                      then IGraph.add_edge igraph d t else ()) ts
                       end) (def (inst));
      InOutSet.iter (IGraph.add_vertex igraph) (def inst);
      InOutSet.iter (IGraph.add_vertex igraph) (use inst);
  end in
  List.iter iter_inst insts;
  igraph

let show_interference (g: igraph): unit =
  IGraph.iter_vertex (fun t ->
      print_string t;
      print_string ": ";
      print_string (String.concat "," (IGraph.succ g t));
      print_string " \n";
    ) g
