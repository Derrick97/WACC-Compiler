%{
open Ast

(* This is the function to convert string to symbol,
 * not the type `symbol` *)
let symbol = Symbol.symbol

(* check if stmt is returnable. useful for checking func returns *)
let rec stmt_returnable = function
  | ExitStmt _ | RetStmt _ -> true
  | WhileStmt (pred, body, _) -> (stmt_returnable body)
  | IfStmt (pred, tpart, epart, pos) -> (stmt_returnable tpart)
                                      && (stmt_returnable epart) (* conditional must return on both branch *)
  | SeqStmt (s::ss) -> (stmt_returnable s) || (stmt_returnable (SeqStmt ss) )
  | _ -> false

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
%token <bool> BOOL
%token <char> CHAR
%token <string> STRING

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

%type <Ast.t> prog
%type <Ast.exp> array_elem

%%
(* prog is the main entry in the parser *)
prog:
| BEGIN; stat; END; EOF; { ([], $2):Ast.t }
| BEGIN; funcs; stat; END; EOF; { ($2, $3):Ast.t }


(* function declarations *)
func:
| typ=typ; name=ID; LPAREN; pl = param_list;
  RPAREN; IS; body=stat; END { (if stmt_returnable(body) == false
                                then raise (SyntaxError ("Not returnable"))
                                else ();
                                FuncDec (typ, symbol name, pl, body, $startpos)) }

funcs:
| func        { [$1] }
| fs=funcs f=func { fs @ [f] }

param:
| typ=typ; id = ID { (typ, symbol id) }

param_list:
| (* can be empty *) { [] }
| p = param; { [p] }
| p = param; COMMA; pl = param_list { p :: pl }

stat:
| typ ID EQ rhs=assign_rhs                  { VarDeclStmt  ($1, symbol $2, rhs, $startpos) }
| SKIP;                                     { SkipStmt     $startpos                                        }
| READ; assign_lhs;                         { ReadStmt    ($2, $startpos)            }
| FREE; expr;                               { FreeStmt    ($2, $startpos)                                   }
| PRINT expr                                { PrintStmt   ($2, $startpos)                                   }
| PRINTLN expr                              { PrintLnStmt ($2, $startpos)                                   }
| RETURN  expr                              { RetStmt     ($2, $startpos)                                   }
| EXIT    expr                              { ExitStmt    ($2, $startpos)                                   }
| lhs = assign_lhs; EQ; rhs = assign_rhs;   { AssignStmt  (lhs, rhs, $startpos)                             }
| BEGIN; s=stat; END                        { BlockStmt   (s, $startpos)                                                       }
| WHILE; exp = expr; DO; s = stat; DONE     { WhileStmt (exp, s, $startpos)                                 }
| IF pred=expr THEN thenp=stat ELSE elsep=stat FI  { IfStmt (pred, thenp, elsep, $startpos)                 }
| fst=stat; SEMICOLON; rest=stat;           { match rest with
                                              | SeqStmt l -> SeqStmt (fst::l)
                                              | _ -> SeqStmt (fst :: [rest])
                                            }
| fst=stat; SEMICOLON;                      { fst                                         }

%inline ident:
| ID { symbol $1 }

(* Assignment *)
assign_lhs:
| id = ID;         { IdentExp (symbol id, $startpos) }
| array_elem;      { $1 }
| pair_elem_id;    { $1 }

assign_rhs:
| expr { $1 }
| array_liter { $1 }
| CALL ID LPAREN args=arg_list RPAREN { CallExp (symbol $2, args, $startpos)}
| CALL ID LPAREN RPAREN { CallExp (symbol $2, [], $startpos) }
| NEWPAIR LPAREN e1=expr COMMA e2=expr RPAREN { NewPairExp (e1, e2, $startpos)}
| pair_elem { $1 }

array_elem:
| name=ID; acc=array_access { ArrayIndexExp (symbol name, acc, $startpos) }

array_access:
| LBRACKET expr RBRACKET { [$2] }
| LBRACKET expr RBRACKET array_access { $2 :: $4 }

pair_elem:
| FST; exp = expr; { FstExp (exp, $startpos) }
| SND; exp = expr; { SndExp (exp, $startpos) }

%inline pair_elem_id:
| FST; ident; { FstExp (IdentExp ($2, $startpos), $startpos) }
| SND; ident; { SndExp (IdentExp ($2, $startpos), $startpos) }

arg_list:
| expr {[$1]}
| exp = expr; COMMA; al = arg_list; {exp::al}

(* Literals *)
array_liter:                    (* TODO implement *)
| LBRACKET RBRACKET { LiteralExp (LitArray [], $startpos) }
| LBRACKET expr RBRACKET { LiteralExp (LitArray [$2], $startpos) }
| LBRACKET expr COMMA other_item RBRACKET { LiteralExp (LitArray ($2::$4), $startpos) }

other_item:                     (* helper for array literals *)
| expr { [$1] }
| expr COMMA other_item { $1 :: $3 }

pair_liter:
| NULL { Null }

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

%inline unary_op:
| BANG  { NotOp }
| MINUS { NegOp }
| LEN   { LenOp }
| ORD   { OrdOp }
| CHR   { ChrOp }

%inline sign:                   (* use inline so that it will force parse to int_liter before expr *)
| PLUS  { "+" }
| MINUS { "-" }

int_liter:
| s=sign; i=INT { match s with
                  | "+" -> i
                  | "-" -> -i }
| i=INT         { i }

expr:
| ID                          { IdentExp (symbol $1, $startpos)                       }
| array_elem                  { $1                                                    }
| i=int_liter                 { check_int_overflow i;
                                LiteralExp (LitInt i, $startpos)                      }
| bool_liter                  { LiteralExp (LitBool $1, $startpos)                    }
| pair_liter                  { LiteralExp ($1, $startpos)                            }
| CHAR;                       { LiteralExp (LitChar $1, $startpos)                    }
| STRING;                     { LiteralExp (LitString $1, $startpos)                  }
| expr PLUS expr              { BinOpExp ($1, PlusOp,   $3, $startpos)                }
| expr MINUS expr             { BinOpExp ($1, MinusOp,  $3, $startpos)                }
| expr TIMES expr             { BinOpExp ($1, TimesOp,  $3, $startpos)                }
| expr DIV expr               { BinOpExp ($1, DivideOp, $3, $startpos)                }
| expr GT expr                { BinOpExp ($1, GtOp,     $3, $startpos)                }
| expr LT expr                { BinOpExp ($1, LtOp,     $3, $startpos)                }
| expr GE expr                { BinOpExp ($1, GeOp,     $3, $startpos)                }
| expr LE expr                { BinOpExp ($1, LeOp,     $3, $startpos)                }
| expr EEQ expr               { BinOpExp ($1, EqOp,     $3, $startpos)                }
| expr NE expr                { BinOpExp ($1, NeOp,     $3, $startpos)                }
| expr AND expr               { BinOpExp ($1, AndOp,    $3, $startpos)                }
| expr OR expr                { BinOpExp ($1, OrOp,     $3, $startpos)                }
| expr MOD expr               { BinOpExp ($1, ModOp,    $3, $startpos)                }
| unary_op; expr;             { UnOpExp  ($1, $2, $startpos)                          }
| LPAREN expr RPAREN          { $2                                                    }


%%
(* Local Variables: *)
(* eval: (merlin-mode -1) *)
(* eval: (electric-indent-mode -1)*)
(* End: *)

