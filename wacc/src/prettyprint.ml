open Ast_v2;;
open Format;;
type fmt = Format.formatter;;

let rec prettyprint_type (fmt:fmt): ty -> unit = function
  | BoolTy -> fprintf fmt "bool"
  | ArrayTy t -> begin
      prettyprint_type fmt t;
      fprintf fmt "[]"
    end
  | PairTyy -> fprintf fmt "pair"
  | IntTy -> fprintf fmt "int"
  | PairTy (t1, t2) -> begin
      fprintf fmt "pair(";
      prettyprint_type fmt t1;
      fprintf fmt ", ";
      prettyprint_type fmt t2;
      fprintf fmt ")"
    end
  | CharTy -> fprintf fmt "char"
  | StringTy -> fprintf fmt "string"
  | NullTy -> fprintf fmt "pair"
and prettyprint_literal fmt l =
  match l with
  | LitString str -> fprintf fmt "%s" ("\"" ^ str ^ "\"")
  | LitBool b -> pp_print_bool fmt b
  | LitChar c -> fprintf fmt "%s" ("\'" ^ (Char.escaped c) ^ "\'")
  | LitInt i  -> pp_print_int fmt i
  | LitArray exps -> begin
      (* fprintf fmt "%s"; *)
      (* ("[" String.concat ", " (List.map (pp_exp fmt) exps) ^ "]") *)
    end
  | LitPair (fst_exp, snd_exp) -> begin
      fprintf fmt "(";
      pp_exp fmt fst_exp;
      fprintf fmt ", ";
      pp_exp fmt snd_exp;
      fprintf fmt ")"
    end
  | LitNull -> fprintf fmt "null"
(* pretty print expressions *)
and prettyprint_binop fmt = function
  | PlusOp   -> fprintf fmt "+"                    (* + *)
  | MinusOp  -> fprintf fmt "-"                    (* - *)
  | TimesOp  -> fprintf fmt "*"                    (* * *)
  | DivideOp -> fprintf fmt "/"                    (* / *)
  | GeOp     -> fprintf fmt ">="                   (* >= *)
  | GtOp     -> fprintf fmt ">"                    (* > *)
  | EqOp     -> fprintf fmt "=="                   (* == *)
  | NeOp     -> fprintf fmt "!="                   (* != *)
  | LtOp     -> fprintf fmt "<"                    (* < *)
  | LeOp     -> fprintf fmt "<="                   (* <= *)
  | AndOp    -> fprintf fmt "&&"                   (* && *)
  | OrOp     -> fprintf fmt "||"                   (* || *)
  | ModOp    -> fprintf fmt "mod"                  (* mod *)
and prettyprint_unop fmt = function
  | NotOp -> fprintf fmt "!"                         (* ! *)
  | NegOp -> fprintf fmt "-"                         (* - *)
  | LenOp -> fprintf fmt "len"                       (* len *)
  | OrdOp -> fprintf fmt "ord"                       (* ord *)
  | ChrOp -> fprintf fmt "chr"                       (* chr *)
and pp_exp fmt (exp, _): unit = prettyprint_exp fmt exp
and prettyprint_exp fmt (e: exp'): unit =
  match e with
  | IdentExp       name -> pp_print_string fmt name
  | LiteralExp     literal -> prettyprint_literal fmt literal
  | BinOpExp      (exp, binop, exp') -> begin
      pp_exp fmt exp;
      print_space();
      prettyprint_binop fmt binop;
      print_space();
      pp_exp fmt exp'
    end
  | UnOpExp       (unop, exp) -> begin
      prettyprint_unop fmt unop;
      print_space();
      pp_exp fmt exp
    end
  | NullExp        -> fprintf fmt "null"
  | NewPairExp    (exp, exp') -> begin
      fprintf fmt "newpair(";
      pp_exp fmt exp;
      fprintf fmt ", ";
      pp_exp fmt exp';
      fprintf fmt ")"
    end
  | CallExp       (fname, exps) -> begin
      fprintf fmt "call ";
      pp_print_string fmt fname;
      fprintf fmt "(";
      (* fprintf fmt "%s" ( String.concat ", " (List.map (pp_exp fmt) exps) ^ ")") *)
    end
  | ArrayIndexExp (name, exps) -> begin
      pp_print_string fmt name
    end
  | FstExp         exp -> begin
      fprintf fmt "fst ";
      pp_exp fmt exp
    end
  | SndExp         exp -> begin
      fprintf fmt "snd ";
      pp_exp fmt exp
    end
and pp_stmt fmt (stmt, _) = prettyprint_stmt fmt stmt
and prettyprint_stmt (fmt:fmt) stmt: unit =
  match stmt with
  | AssignStmt    (lhs, rhs) -> begin
      pp_exp fmt lhs;
      fprintf fmt "=";
      pp_exp fmt rhs
    end
  | IfStmt        (pred, then_, else_) -> begin
      fprintf fmt "if (";
      (pp_exp fmt pred);
      fprintf fmt ") then ";
      (pp_stmt fmt then_);
      fprintf fmt "else ";
      (pp_stmt fmt else_)
    end
  | WhileStmt     (pred, body) -> begin
      fprintf fmt "while";
      print_break 1 0;
      pp_exp fmt pred;
      print_break 1 0;
      fprintf fmt "{";
      open_hovbox 4;
      pp_stmt fmt body;
      close_box();
      fprintf fmt "}"
    end
  | ExitStmt      (exp) ->  begin
      fprintf fmt "exit ";
      pp_exp fmt exp
    end
  | VarDeclStmt   (ty, name, exp) -> begin
      prettyprint_type fmt ty;
      print_break 1 0;
      pp_print_string fmt name;
      print_break 1 0;
      fprintf fmt "=";
      print_space();
      pp_exp fmt exp
    end
  | PrintStmt     (newline, exp) -> begin
      (if newline then (
          fprintf fmt "println";
          print_break 1 0;
          pp_exp fmt exp)
       else (fprintf fmt "print";
             print_break 1 0;
             pp_exp fmt exp))
    end
  | RetStmt      (exp) -> begin
      fprintf fmt "return";
      pp_exp fmt exp
    end
  | SeqStmt      (stmt, stmtlist) -> begin
      open_hovbox 2;
      pp_stmt fmt stmt;
      fprintf fmt ";";
      close_box ();
      print_newline();
      (pp_stmt fmt stmtlist)
    end
  | ReadStmt     exp -> fprintf fmt "read "; pp_exp fmt exp
  | FreeStmt     exp -> fprintf fmt "free "; pp_exp fmt exp
  | SkipStmt     -> fprintf fmt "skip"
  | BlockStmt   block -> begin
      open_box 2;
      fprintf fmt "begin";
      print_newline();
      (pp_stmt fmt block);
      fprintf fmt "end";
      close_box()
    end
