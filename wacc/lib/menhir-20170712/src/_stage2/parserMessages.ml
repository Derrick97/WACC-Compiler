
(* This file was auto-generated based on "parserMessages.messages". *)

(* Please note that the function [message] can raise [Not_found]. *)

let message =
  fun s ->
    match s with
    | 54 | 66 | 57 | 62 | 58 | 70 ->
        "Ill-formed %attribute declaration.\nAn %attribute declaration should contain a nonempty list of symbols,\nfollowed with a nonempty list of attributes.\nExamples of well-formed declarations:\n  %attribute FOO [@printer \"foo\"]\n  %attribute bar BAZ [@printer \"bar/BAZ\"] [@cost 2.0]\n"
    | 11 ->
        "Ill-formed list of actual parameters.\nA modifier, a closing parenthesis, or a comma is expected at this point.\nA modifier is * or + or ?.\nExamples of well-formed actual parameters:\n  expr\n  expr+\n  option(expr)\n  separated_list(COMMA, expr)\n"
    | 10 | 127 ->
        "Ill-formed list of actual parameters.\nA comma-delimited list of actual parameters is expected at this point.\nExamples of well-formed actual parameters:\n  expr\n  expr+\n  option(expr)\n  separated_list(COMMA, expr)\n"
    | 179 ->
        "Either another rule\nor another production | ...\nis expected at this point.\nExamples of well-formed rules:\n  option(X): { None } | x = X { Some x }\n"
    | 114 ->
        "Ill-formed rule.\nA colon is expected at this point.\nExamples of well-formed rules:\n  option(X): { None } | x = X { Some x }\n"
    | 140 ->
        "Ill-formed rule.\nEither a semantic action { ... }\nor another production | ...\nis expected here.\nExamples of well-formed rules:\n  expr: MINUS e = expr %prec UMINUS { -e }\n"
    | 123 | 159 ->
        "Ill-formed rule.\nA comma-delimited list of actual parameters is expected at this point.\nExamples of well-formed rules:\n  call: f = callee LPAREN args = separated_list(COMMA, expr) RPAREN { f, args }\n  list(X): { [] } | x = X; xs = list(X) { x :: xs }\n"
    | 154 | 133 | 175 ->
        "Ill-formed %prec annotation.\nA symbol is expected at this point.\nExamples of well-formed annotations:\n  expr: MINUS e = expr %prec UMINUS { -e }\n"
    | 132 | 137 ->
        "Either another production | ...\nor a comma or a closing parenthesis\nis expected at this point.\n"
    | 126 | 165 | 166 | 125 | 138 ->
        "Ill-formed production.\nMaybe you meant to close a parenthesis at this point?\nA production is a sequence of producers, followed with a semantic action.\nExamples of well-formed producers:\n  expr\n  option(COMMA)\n  separated_list(COMMA, expr)\n  e = expr\n  ds = declaration*\n  es = list(terminated(expr, SEMI))\n  es = list(e = expr SEMI { e })\n  xs = list(x = var { Some x } | WILDCARD { None })\n"
    | 122 | 149 | 119 | 120 | 145 | 170 ->
        "Ill-formed production.\nA production is a sequence of producers, followed with a semantic action.\nExamples of well-formed producers:\n  expr\n  option(COMMA)\n  separated_list(COMMA, expr)\n  e = expr\n  ds = declaration*\n  es = list(terminated(expr, SEMI))\n  es = list(e = expr SEMI { e })\n  xs = list(x = var { Some x } | WILDCARD { None })\n  expr [@cost 0]\n"
    | 115 | 117 | 180 | 141 ->
        "Ill-formed rule.\nA list of productions is expected at this point.\nExamples of well-formed rules:\n  main: e = expr EOL { e }\n  expr: i = INT { i } | e1 = expr PLUS e2 = expr { e1 + e2 }\n  symbol: s = LID | s = UID { s }\n"
    | 106 | 109 | 110 ->
        "Ill-formed rule.\nA comma-delimited list of formal parameters is expected at this point.\nExamples of well-formed rules:\n  option(X): { None } | x = X { Some x }\n  pair(X, Y): x = X; y = Y { (x, y) }\n"
    | 102 | 103 ->
        "Ill-formed rule.\nEither a parenthesized, comma-delimited list of formal parameters\nor an attribute\nor a colon is expected at this point.\nExamples of well-formed rules:\n  main: e = expr EOL { e }\n  expr: i = INT { i } | e1 = expr PLUS e2 = expr { e1 + e2 }\n  option(X): { None } | x = X { Some x }\n  main [@cost 0]: e = expr EOL { e }\n"
    | 93 | 95 | 99 ->
        "Ill-formed rule.\n%inline, %public, or a non-terminal symbol is expected at this point.\nExamples of well-formed rules:\n  %public option(X): { None } | x = X { Some x }\n  %inline clist(X): xs = separated_nonempty_list(COMMA?, X) { xs }\n  %public %inline pair(X, Y): x = X; y = Y { (x, y) }\n"
    | 174 ->
        "Either another rule or %% is expected at this point.\n"
    | 92 ->
        "Either a rule or %% is expected at this point.\n"
    | 73 ->
        "Ill-formed %parameter declaration.\nExamples of well-formed declarations:\n  %parameter <X : sig type t end>\n"
    | 84 | 87 | 88 ->
        "Ill-formed precedence declaration.\nExamples of well-formed declarations:\n  %left PLUS\n  %left PLUS MINUS\n  %nonassoc unary_minus\n  %right CONCAT\n"
    | 44 | 46 | 47 | 49 ->
        "Ill-formed %start declaration.\nA start symbol must begin with a lowercase letter.\nExamples of well-formed declarations:\n  %start program\n  %start expression phrase\n  %start <int> date time\n"
    | 33 | 35 | 36 | 41 | 37 ->
        "Ill-formed %token declaration.\nExamples of well-formed declarations:\n  %token FOO\n  %token DOT SEMICOLON\n  %token <string> LID UID\n  %token FOO [@cost 0]\n"
    | 1 | 2 | 5 | 6 | 29 | 9 | 15 | 24 | 75 ->
        "Ill-formed declaration.\nExamples of well-formed declarations:\n  %type <Syntax.expression> expression\n  %type <int> date time\n  %type <int option> option(date)\n  %on_error_reduce expression\n  %on_error_reduce date time\n  %on_error_reduce option(date)\n"
    | 0 | 187 ->
        "Either a declaration or %% is expected at this point.\n"
    | _ ->
        raise Not_found
