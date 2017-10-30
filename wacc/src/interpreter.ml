open Ast
module A = Ast

type wrapType =
  | Int of int
  | Char of char
  | Bool of bool
  | String of string
  | NullType
  | ListOfElem of int * A.exp list
  | PairsTy of A.exp * A.exp
  | DefaultTy
  | ErrorTy

let stack: wrapType Stack.t = (Stack.create ())
let table = ref (Symbol.empty)

let replaceList index elems newItem = (
  let finalList = [] in
  for i = 0 to (List.length elems)-1 do
    if(i = index) then newItem::finalList
    else finalList::(List.nth elems i)
  done
)

let printWrap wrapTy =
  match wrapTy with
  | Int(num) -> print_int num
  | Char(letter) -> print_char letter
  | Bool(tf) -> if tf then print_string "true" else print_string "false"
  | String(str) -> print_string str
  | NullType -> print_string "nil"
  | ListOfElem(_,_) -> print_string "0x12008"
  | PairsTy(_,_) -> print_string "0x22008"
  | DefaultTy -> print_string "Will Fix"
  | ErrorTy -> print_string "ERROR CODE"

let arithmeticConvert wrapTy =
  match wrapTy with
  | Int(num) -> num
  | _ -> -1                   (*TODO:Need Revision here to make it throw an exception*)

let booleanConvert wrapTy =
  match wrapTy with
  | Bool(tf) -> tf
  | _ -> false

  (*Test Code*)
let rec compute exp table = match exp with
| A.IdentExp (symbol, _) -> Symbol.lookup symbol !table
| A.LiteralExp(lite, _) -> (match lite with
  | A.LitString(str) -> String(str)
  | A.LitBool(boo) -> Bool(boo)
  | A.LitChar(cha) -> Char(cha)
  | A.LitInt (num) -> Int(num)
  | A.LitArray (expList) -> (
    let length = List.length expList in
    ListOfElem(length, expList);
    )
  | A.LitPair -> NullType
  | A.Null -> NullType)
| A.BinOpExp (ex1, binop, ex2, _) -> (match binop with
  | A.PlusOp -> Int(arithmeticConvert (compute ex1 table) + arithmeticConvert(compute ex2 table))
  | A.MinusOp -> Int(arithmeticConvert (compute ex1 table) - arithmeticConvert(compute ex2 table))
  | A.TimesOp -> Int(arithmeticConvert (compute ex1 table) * arithmeticConvert(compute ex2 table))
  | A.DivideOp -> Int(arithmeticConvert (compute ex1 table) / arithmeticConvert(compute ex2 table))
  | A.GeOp -> Bool (arithmeticConvert (compute ex1 table) >= arithmeticConvert(compute ex2 table))
  | A.GtOp -> Bool (arithmeticConvert (compute ex1 table) > arithmeticConvert(compute ex2 table))
  | A.EqOp -> Bool (arithmeticConvert (compute ex1 table) = arithmeticConvert(compute ex2 table))
  | A.NeOp -> Bool (arithmeticConvert (compute ex1 table) != arithmeticConvert(compute ex2 table))
  | A.LtOp -> Bool (arithmeticConvert (compute ex1 table) < arithmeticConvert(compute ex2 table))
  | A.LeOp -> Bool (arithmeticConvert (compute ex1 table) <= arithmeticConvert(compute ex2 table))
  | A.AndOp -> Bool (booleanConvert (compute ex1 table) && booleanConvert(compute ex2 table))
  | A.OrOp -> Bool (booleanConvert (compute ex1 table)|| booleanConvert(compute ex2 table))
  | A.ModOp -> Int(arithmeticConvert (compute ex1 table) mod arithmeticConvert(compute ex2 table)))
| A.UnOpExp (unop, exp, _) -> (
  let operand = compute exp table in
  match unop with
  | A.NotOp -> Bool(not(booleanConvert operand));
  | A.NegOp -> Int(0-(arithmeticConvert operand));
  | A.LenOp -> (match operand with
    | ListOfElem(length, expList) -> Int(length)
    | String(str) -> Int(String.length str)
    | _ -> Int(-1);
    )
  | A.OrdOp -> (match operand with
    | Char(cha) -> Int(Char.code cha)
    | Int(num) -> Int(num)
    | _ -> Int(256);
    )
  | A.ChrOp -> (match operand with
    | Int(num) -> Char(Char.chr num)
    | _ -> Char('?');
  ))
| A.ArrayIndexExp(symbol, explist,_) -> ( match (Symbol.lookup symbol !table) with
  | ListOfElem(length, elems) -> indexList (ListOfElem(length, elems)) explist
  | _ -> ErrorTy)
| A.NewPairExp(ex1,ex2) -> PairsTy(ex1, ex2)
| A.FstExp(exp,_) -> (match compute exp table with
  | PairsTy(ex1,_) -> compute ex1 table
  | _ -> ErrorTy)
| A.SndExp(exp,_) -> (match compute exp table with
  | PairsTy(_,ex2) -> compute ex2 table
  | _ -> ErrorTy)

and indexList (ListOfElem(length,elems)) indexExps =
  match indexExps with
  | index::[] -> compute (List.nth elems (arithmeticConvert (compute index table))) table
  | h::r -> indexList (compute (List.nth elems (arithmeticConvert (compute h table))) table) r



  let rec matchLHS lhs rhs = match lhs with
  | A.IdentExp(symbol, _) -> symbol
  | A.FstExp(exp, _) -> (match exp with
    | A.IdentExp(symbol,_) -> (
      let res = compute exp table in
      match res with
      | PairsTy(ex1,ex2) -> (table := Symbol.insert symbol (PairsTy(rhs,ex2)) (!table); symbol;)
      | _ -> (symbol;) (*Need to throw errors*)
      )
    | A.ArrayIndexExp(symbol,explist,_) -> (
      let elemList = Symbol.lookup symbol !table in
      let value = indexList elemList explist in
      table := Symbol.insert symbol value (!table);
      symbol;
      )
    | A.FstExp(exps,_) -> matchLHS exps rhs
    | A.SndExp(exps,_) -> matchLHS exps rhs
    | A.NullExp -> (
      table := Symbol.insert "nil" (NullType) (!table);
      Symbol.symbol "nil";)
    | _ -> Symbol.symbol "what"
    )
  | A.SndExp(exp,_) -> (match exp with
    | A.IdentExp(symbol,_) -> (let res = compute exp table in
      match res with
      | PairsTy(ex1,ex2) -> (table := Symbol.insert symbol (PairsTy(ex1,rhs)) (!table); symbol;)
      | _ -> (symbol;) (*Need to throw errors*)
    )
    | A.ArrayIndexExp(symbol,_,_) -> ("snd[]" ^ symbol))
    | A.FstExp(exps,_) -> matchLHS exps rhs
    | A.SndExp(exps,_) -> matchLHS exps rhs
    | A.NullExp -> (
      table := Symbol.insert "nil" (NullType) (!table);
      Symbol.symbol "nil";)
    | _ -> Symbol.symbol "what"
  | A.ArrayIndexExp(symbol,explist,_) -> (
    let elemList = Symbol.lookup symbol !table in

    symbol;
    )


  let rec eval singleStmt = (match singleStmt with
    | A.SeqStmt (stmt, stmtlist) -> (eval stmt; eval stmtlist;)
    | A.SkipStmt(_) ->  ()
    | A.VarDeclStmt(_,sym,exp,_) ->(
      let value = compute exp table in
      table := Symbol.insert sym value !table;
      Stack.push value stack;
      )
    | A.AssignStmt(lhs, rhs, _) ->(
      let value = compute rhs table in
      table := Symbol.insert (matchLHS lhs rhs) value !table;
      Stack.push value stack;
      )
    | A.PrintStmt(exp, _) -> (
        Stack.push (compute exp table) stack;
        let result = Stack.pop stack in
        printWrap result;
        )
    | A.PrintLnStmt(exp, _) ->(
        Stack.push (compute exp table) stack;
        let result = Stack.pop stack in
        printWrap result;
        print_newline ();
        )
    | A.WhileStmt(exp, stmt) -> (
        let cond = compute exp table in
          match cond with
          | Bool(true) -> (eval stmt; eval (A.WhileStmt(exp ,stmt));)
          | Bool(false) -> ();
          | _ -> print_string ("This is not a boolean expression, cannot used for condition.");
      )
    | A.IfStmt(exp,stmt1,stmt2,_) -> (
      let cond = compute exp table in
        match cond with
        | Bool(true) -> eval stmt1
        | Bool(false) -> eval stmt2
        | _ -> print_string ("This is not a boolean expression, cannot used for condition.");
      )
    | A.BlockStmt(statment, _) ->
        eval statment)
