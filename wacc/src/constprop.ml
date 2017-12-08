(* dataflow analysis *)
module CFG = Cfg;;
module IL = Il;;

module Int = struct
  type t = int
  let compare = Pervasives.compare
end



module ReachSet = Set.Make(Int);;

type il = IL.il;;
type graph = CFG.t
type reachset = ReachSet.t

let debug = false

let string_of_set s =
  ("{" ^ (String.concat "," (List.map string_of_int (ReachSet.elements s))) ^ "}")

let print_set s = (print_endline (string_of_set s))

let int_of_string_opt s =
  try
    let i = int_of_string s in Some i
  with
  | _ -> None

(** compute definitions of a temporary in instruction *)
let defs
    (cfg:CFG.t)
    (t:Liveness.InOutSet.elt): ReachSet.t =
  CFG.fold_vertex (fun (is, i) s ->
      if (Liveness.InOutSet.mem t (Liveness.def (is, i) ))
      then ReachSet.union s (ReachSet.of_list [i])
      else s) cfg ReachSet.empty

let gen (cfg:CFG.t) is = begin
  let open IL in
  let is, i = is in
  ReachSet.of_list (match is with
      | ADD   (dst, _, _)
      | SUB   (dst, _, _)
      | DIV   (dst, _, _)
      | AND   (dst, _, _)
      | EOR   (dst, _, _)
      | ORR   (dst, _, _)
      | CMP   (_, dst, _, _) -> [i]
      | LOAD  (_, dst, _) -> [i]
      | MOV (t, _) -> [i]
      | CALL _ -> [i]
      | MUL   (dst, _, _) -> [i]
      | POP _ -> []
      | STORE _ -> []
      | PUSH _ | JUMP _ | LTORG | COMP _ | CBR _ | RET _ | LABEL _ | NOOP -> [])
end

let kill (cfg:CFG.t) (is:(il*int)) :ReachSet.t  = begin
  let open IL in
  let open ReachSet in
  let is, i = is in
  (match is with
   | ADD   (dst, _, _)
   | SUB   (dst, _, _)
   | DIV   (dst, _, _)
   | AND   (dst, _, _)
   | EOR   (dst, _, _)
   | ORR   (dst, _, _)
   | MUL   (dst, _, _)
   | CMP   (_, dst, _, _)
   | LOAD  (_, dst, _) | MOV (dst, _) -> diff (defs cfg (dst)) (of_list [i])
   | CALL _ -> diff (defs cfg (Arm.reg_RV)) (of_list [i])
   | POP _ | STORE _
   | PUSH _ | JUMP _ | LTORG | COMP _ | CBR _ | RET _ | LABEL _ | NOOP  -> empty)
end

(** build a livenes graph *)
let build_reach (instrs: (il*int) list) = begin
  (* solve the dataflow equations
     See modern compiler implementation in ML, page 214 Algo 10.4 for pseudo-code.
  *)
  (* first we build the control flow graph *)
  let cfg = CFG.build_cfg instrs in

  let (gens: (CFG.V.t, ReachSet.t) Hashtbl.t) = Hashtbl.create 0 in
  let (kills: (CFG.V.t, ReachSet.t) Hashtbl.t) = Hashtbl.create 0 in
  let (reachin: (CFG.V.t, ReachSet.t) Hashtbl.t) = Hashtbl.create 0 in
  let (reachout: (CFG.V.t, ReachSet.t) Hashtbl.t) = Hashtbl.create 0 in

  List.iter (fun i ->
      Hashtbl.add gens i (gen cfg i);
      Hashtbl.add kills i (kill cfg i)) instrs;

  if debug then
    List.iter (fun v ->
        print_int (snd v); print_string "\t";
        print_endline (IL.show_il (fst v));
        print_string "gens\t"; print_set (Hashtbl.find gens v);
        print_string "kills\t"; print_set (Hashtbl.find kills v)
      ) instrs else ();

  (* initialize *)
  CFG.iter_vertex (fun n ->
      let (_, i) = n in
      Hashtbl.replace reachin n (ReachSet.empty);
      Hashtbl.replace reachout n (ReachSet.empty);
    ) cfg;

  (* now iteratively solve *)
  let i = ref 0 in
  let rec loop () = begin
    let terminate = ref true in
    i := !i + 1;
    let open Printf in
    List.iter (fun n -> begin
          let in'  = Hashtbl.find reachin n in       (* in'[n] <- in[n] *)
          let out' = Hashtbl.find reachout n in      (* out'[n] <- out[n] *)
          let new_out = ReachSet.(union
                                    (Hashtbl.find gens n)
                                    (diff in' (Hashtbl.find kills n))) in  (* out[n] = gen[n] U (in[n] - kill[n]) *)
          let new_in = List.fold_left
              (fun a b -> ReachSet.union a (Hashtbl.find reachout b))
              (ReachSet.empty) ((CFG.pred cfg n)) in (* in[n] = U_{p \in pred(n)} out[s] *)
          if not((ReachSet.equal in' new_in) && (ReachSet.equal out' new_out)) then
            terminate := false else ();
          Hashtbl.replace reachin  n new_in;
          Hashtbl.replace reachout n new_out;
        end) instrs;
    if not !terminate then loop () else ()
  end in
  loop ();

  if debug then (
    print_endline "-----------------";
    List.iter (fun v ->
        print_int (snd v); print_string "\t";
        print_endline (IL.show_il (fst v));
        print_string "In\t"; print_set (Hashtbl.find reachin v);
        print_string "Out\t"; print_set (Hashtbl.find reachout v)
      ) instrs) else ();
  reachin, reachout
end

let option_mem = function
  | Some _ -> true
  | None -> false


let constant_prop
    (insts: (il*int) list)
    (reachins: (CFG.V.t, ReachSet.t) Hashtbl.t) = begin
  let open IL in
  let reaching_def (ins, i) = begin
    let ds = ReachSet.elements (Hashtbl.find reachins (ins, i)) in
    let reach_def i = (match List.nth insts i with
                       | MOV (t, op), p -> Some (t, op)
                       | _ -> None) in
    let const_reach = ds
                      |> List.map reach_def
                      |> List.filter option_mem
                      |> List.map (fun (Some e) -> e)
    in
    const_reach
  end
  in
  let maysub (reach_const: (temp*IL.operand) list) =
    let may_sub_with_imm = (function
        | OperReg t0 as op ->
           let matched = (List.filter (fun (t, _) -> t = t0) reach_const) in
           if List.length matched = 1 then
             match (List.hd matched) with
             | (_, OperImm i) -> (OperImm i)
             | _ -> op
            else op
        | op -> op) in function
      | ADD (dst, op0, op1) ->  ADD (dst, may_sub_with_imm op0, may_sub_with_imm op1)
      | SUB (dst, op0, op1) ->  SUB (dst, may_sub_with_imm op0, may_sub_with_imm op1)
      (* | MUL (dst, op0, op1) ->  MUL (dst, may_sub_with_imm op0, may_sub_with_imm op1) *)
      | DIV (dst, op0, op1) ->  DIV (dst, may_sub_with_imm op0, may_sub_with_imm op1)
      | MOV (dst, op) as inst -> let inst' = MOV (dst, may_sub_with_imm op) in
                                 (* print_endline (IL.show_il inst); print_endline (IL.show_il inst'); *) inst'
      | op -> op in
  List.map
    (fun (ins, i) ->
       begin
         let reach = reaching_def (ins, i) in
         (maysub reach ins, i)
       end)
    insts;
end
