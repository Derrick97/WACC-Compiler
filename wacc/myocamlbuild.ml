open Ocamlbuild_plugin;;

let wacclib_command env build =
  let arg = env "wacclib.c" in
  Cmd(S[A"gcc"; A"-S"; P(arg);]);;

(rule "build wacclib"
  ~prod:"wacclib.s"
  ~dep:"wacclib.c"
  ~doc:"build WACC's external library"
  wacclib_command);;

(* Enable ocaml build *)
let () =
  dispatch (function
    | Before_options ->
      Options.use_ocamlfind := true
    | _ -> ())
