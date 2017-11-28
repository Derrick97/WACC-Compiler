%{
open Ast_v2

(* This is the function to convert string to symbol,
 * not the type `symbol` *)
let symbol = Symbol.symbol

(* check if stmt is returnable. useful for checking func returns *)
let rec stmt_returnable: stmt -> bool = function
  | (ExitStmt _, _) | (RetStmt _, _) -> true
  | WhileStmt (pred, body), _ -> (stmt_returnable body)
  | IfStmt (pred, tpart, epart), _ -> (stmt_returnable tpart)
                                      && (stmt_returnable epart) (* conditional must return on both branch *)
  | SeqStmt (stmt, stmtlist), _ -> (stmt_returnable stmt) || (stmt_returnable stmtlist )
  | _ -> false

let check_int_overflow num =
  let max_int = Int32.to_int Int32.max_int in
  let min_int = Int32.to_int Int32.min_int in
  (if num <= max_int && num >= min_int
  then num
  else raise (SyntaxError ("Int overflow: " ^ string_of_int num)));;
%}

%token BANG
%token EOF
%token PAIR
%token NEWPAIR
%token IF FI
%token COMMA
%token FST SND
%token WHILE
%token THEN
%token ELSE
%token TRUE
%token FALSE
%token NULL
%token IS
%token SEMICOLON
%token DO DONE
%token <string> ID
%token LEN
%token ORD
%token CHR

%token PLUS MINUS TIMES DIV MOD
%token INC DEC PLUS_EQ MINUS_EQ TIMES_EQ
%token EQ GT LT GE LE EEQ NE
%token AND OR                   (* && || *)

(* Precedence *)
(* level 6 *)
%left OR
(* level 5 *)
%left AND
(* level 4 *)
%left NE EEQ
(* level 3 *)
%left GT GE LT LE
(* level 2 *)
%left PLUS MINUS
(* level 1 *)
%left TIMES DIV

%token LPAREN RPAREN
%token LBRACKET RBRACKET

%token <int> INT
%token <char> CHAR
%token <string> STRING
%token <int> HEX

%token INTT                     (* int *)
%token BOOLT                    (* bool *)
%token CHART                    (* char *)
%token STRINGT                  (* string *)

%token CALL
%token FREE
%token RETURN
%token EXIT
%token READ
%token PRINT
%token PRINTLN
%token SKIP

%token BEGIN
%token END

%start prog

%type <Ast_v2.t> prog
%type <Ast_v2.exp> array_elem

%%
(* prog is the main entry in the parser *)
prog:
| BEGIN; stat; END; EOF; { ([], $2):Ast_v2.t }
| BEGIN; funcs; stat; END; EOF; { ($2, $3):Ast_v2.t }


(* function declarations *)
func:
| typ=typ; name=ID; LPAREN; pl = param_list;
  RPAREN; IS; body=stat; END { (if not(stmt_returnable body)
                                then raise (SyntaxError ("Not returnable"))
                                else ();
                                (FuncDec (typ, name, pl, body), $startpos)) }

funcs:
| func        { [$1] }
| fs=funcs f=func { fs @ [f] }

param:
| typ=typ; id = ID { (typ, id) }

param_list:
| (* can be empty *) { [] }
| p = param; { [p] }
| p = param; COMMA; pl = param_list { if (pl == []) then raise (SyntaxError "Bad args") else p::pl }

%inline side_effect_op:
| INC  { IncOp }
| DEC  { DecOp }

%inline side_effect_two_args_op:
| PLUS_EQ { PlusEqOp }
| MINUS_EQ { MinusEqOp }
| TIMES_EQ { TimesEqOp }


%inline ident:
| ID { $1 }

stat:
| typ ID EQ rhs=assign_rhs                  { VarDeclStmt($1, $2, rhs), $startpos          }
| SKIP;                                     { SkipStmt, $startpos                          }
| READ; assign_lhs;                         { ReadStmt($2), $startpos                      }
| assign_lhs; side_effect_op;               { SideEffectStmt($1,$2), $startpos             }
| assign_lhs; side_effect_two_args_op; expr          { TwoArgsSideEffectStmt($1,$2,$3), $startpos   }
| FREE; expr;                               { FreeStmt($2), $startpos                      }
| PRINT expr                                { PrintStmt(false, $2), $startpos              }
| PRINTLN expr                              { PrintStmt (true, $2), $startpos              }
| RETURN  expr                              { RetStmt($2), $startpos                       }
| EXIT    expr                              { ExitStmt($2), $startpos                      }
| lhs = assign_lhs; EQ; rhs = assign_rhs;   { AssignStmt(lhs, rhs), $startpos              }
| BEGIN; s=stat; END                        { BlockStmt(s), $startpos                      }
| WHILE; exp = expr; DO; s = stat; DONE     { WhileStmt(exp, s), $startpos                 }
| IF pred=expr THEN thenp=stat ELSE elsep=stat FI  { IfStmt(pred, thenp, elsep), $startpos }
| fst=stat; more=sequential_stmt            { SeqStmt (fst, more), $startpos               }

 sequential_stmt:
| SEMICOLON; rest=stat; { rest }

(* Assignment *)
assign_lhs:
| id = ID;         { IdentExp (id), $startpos }
| array_elem;      { $1 }
| pair_elem_id;    { $1 }

assign_rhs:
| expr { $1 }
| array_liter { $1 }
| CALL ID LPAREN args=arg_list RPAREN { CallExp ($2, args), $startpos}
| CALL ID LPAREN RPAREN { CallExp ($2, []), $startpos }
| NEWPAIR LPAREN e1=expr COMMA e2=expr RPAREN { NewPairExp(e1, e2), $startpos}
| pair_elem { $1 }

array_elem:
| name=ID; acc=array_access { ArrayIndexExp (name, acc), $startpos }

array_access:
| LBRACKET expr RBRACKET { [$2] }
| LBRACKET expr RBRACKET array_access { $2 :: $4 }

pair_elem:
| FST; exp = expr; { FstExp (exp), $startpos }
| SND; exp = expr; { SndExp (exp), $startpos }

%inline pair_elem_id:
| FST; ident; { FstExp ((IdentExp ($2), $startpos)), $startpos }
| SND; ident; { SndExp ((IdentExp ($2), $startpos)), $startpos }

arg_list:
| expr {[$1]}
| exp = expr; COMMA; al = arg_list; {exp::al}

(* Literals *)
array_liter:                    (* TODO implement *)
| LBRACKET RBRACKET { LiteralExp (LitArray []), $startpos }
| LBRACKET expr RBRACKET { LiteralExp (LitArray [$2]), $startpos }
| LBRACKET expr COMMA other_item RBRACKET { LiteralExp (LitArray ($2::$4)), $startpos }

other_item:                     (* helper for array literals *)
| expr { [$1] }
| expr COMMA other_item { $1 :: $3 }

pair_liter:
| NULL { LitNull }

bool_liter:
| TRUE  { true  }
| FALSE { false }

(* Types *)

typ:
| INTT       { IntTy                        }
| BOOLT      { BoolTy                       }
| STRINGT    { StringTy                     }
| CHART      { CharTy                       }
| array_type { $1                           }
| pair_type  { $1                           }

base_type:
| INTT    { IntTy    }
| BOOLT   { BoolTy   }
| CHART   { CharTy   }
| STRINGT { StringTy }

array_type:
| t=typ; LBRACKET; RBRACKET; { ArrayTy t }

pair_type:
| PAIR; LPAREN; fst_t=pair_elem_type; COMMA; snd_t=pair_elem_type; RPAREN; { PairTy (fst_t, snd_t) }

%inline pair_elem_type:
| PAIR;      { PairTyy }
| array_type { $1 }
| base_type  { $1 }

%inline sign:                   (* use inline so that it will force parse to int_liter before expr *)
| PLUS  { "+" }
| MINUS { "-" }

int_liter:
| s=sign; i=INT { match s with
                  | "+" -> i
                  | "-" -> -i
                  | _   -> assert false }
| i=INT         { i }

hex_literal:
| h=HEX { h }

%inline unary_op:
| BANG  { NotOp }
| MINUS { NegOp }
| LEN   { LenOp }
| ORD   { OrdOp }
| CHR   { ChrOp }


%inline binary_op:
| PLUS              { PlusOp         }
| MINUS             { MinusOp        }
| TIMES             { TimesOp        }
| DIV               { DivideOp       }
| GT                { GtOp           }
| LT                { LtOp           }
| GE                { GeOp           }
| LE                { LeOp           }
| EEQ               { EqOp           }
| NE                { NeOp           }
| AND               { AndOp          }
| OR                { OrOp           }
| MOD               { ModOp          }

expr:
| ID                          { IdentExp ($1), $startpos               }
| array_elem                  { $1                                     }
| i=int_liter                 { check_int_overflow i;
                                LiteralExp (LitInt i), $startpos       }
| bool_liter                  { LiteralExp (LitBool $1), $startpos     }
| hex_literal                 { check_int_overflow $1;
                                LiteralExp (LitInt $1), $startpos       }
| pair_liter                  { NullExp, ($startpos)                   }
| CHAR;                       { LiteralExp (LitChar $1), $startpos     }
| STRING;                     { LiteralExp (LitString $1), $startpos   }
| unary_op; expr;             { UnOpExp  ($1, $2), $startpos           }
| expr; binary_op; expr       { BinOpExp ($1, $2, $3), $startpos       }
| LPAREN expr RPAREN          { $2                                     }

%%
(* Local Variables: *)
(* eval: (merlin-mode -1) *)
(* eval: (electric-indent-mode -1)*)
(* End: *)
