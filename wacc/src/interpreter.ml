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




let printWrap wrapTy =
  match wrapTy with
  | Int(num) -> print_int num
  | Char(letter) -> print_char letter
  | Bool(tf) -> if tf then print_string "true" else print_string "false"
  | String(str) -> print_string str
  | NullType -> print_string "nil"
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
| A.IdentExp (symbol, _) -> compute (Symbol.lookup symbol !table) table
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
  | A.TimesOp -> Int(arithmeticConvert (compute ex1 table) / arithmeticConvert(compute ex2 table))
  | A.DivideOp -> Int(arithmeticConvert (compute ex1 table) / arithmeticConvert(compute ex2 table))
  | A.GeOp -> Bool (arithmeticConvert (compute ex1 table) >= arithmeticConvert(compute ex2 table))
  | A.GtOp -> Bool (arithmeticConvert (compute ex1 table) > arithmeticConvert(compute ex2 table))
  | A.EqOp -> Bool (arithmeticConvert (compute ex1 table) == arithmeticConvert(compute ex2 table))
  | A.NeOp -> Bool (arithmeticConvert (compute ex1 table) != arithmeticConvert(compute ex2 table))
  | A.LtOp -> Bool (arithmeticConvert (compute ex1 table) < arithmeticConvert(compute ex2 table))
  | A.LeOp -> Bool (arithmeticConvert (compute ex1 table) <= arithmeticConvert(compute ex2 table))
  | A.AndOp -> Bool (booleanConvert (compute ex1 table) && booleanConvert(compute ex2 table))
  | A.OrOp -> Bool (booleanConvert (compute ex1 table)|| booleanConvert(compute ex2 table))
  | A.ModOp -> Int(arithmeticConvert (compute ex1 table) mod arithmeticConvert(compute ex2 table)))
| A.ArrayIndexExp(symbol, explist,_) -> ( match (compute (Symbol.lookup symbol !table) table) with
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
  | A.IdentExp(symbol, _) -> (
    table := Symbol.insert symbol rhs !table;
    symbol;)
  | A.FstExp(exp, _) -> (match exp with
    | A.IdentExp(symbol,_) -> (
      let res = compute exp table in
      match res with
      | PairsTy(ex1,ex2) -> (table := Symbol.insert symbol (NewPairExp(rhs,ex2)) (!table);symbol;)
      | _ -> (symbol;) (*Need to throw errors*)
      )
    | A.ArrayIndexExp(symbol,_,_) -> ("fst[]" ^ (symbol))
    | A.FstExp(exps,_) -> matchLHS exps rhs
    | A.SndExp(exps,_) -> matchLHS exps rhs
    | A.NullExp -> (
      table := Symbol.insert "nil" (NullExp) (!table);
      Symbol.symbol "nil";)
    | _ -> Symbol.symbol "what"
    )
  | A.SndExp(exp,_) -> (match exp with
    | A.IdentExp(symbol,_) -> (let res = compute exp table in
    match res with
    | PairsTy(ex1,ex2) -> (table := Symbol.insert symbol (NewPairExp(ex1,rhs)) (!table);symbol;)
    | _ -> (symbol;) (*Need to throw errors*)
    )
    | A.ArrayIndexExp(symbol,_,_) -> ("snd[]" ^ symbol))
    | A.FstExp(exps,_) -> matchLHS exps rhs
    | A.SndExp(exps,_) -> matchLHS exps rhs
    | A.NullExp -> (
      table := Symbol.insert "nil" (NullExp) (!table);
      Symbol.symbol "nil";)
    | _ -> Symbol.symbol "what"
  | A.ArrayIndexExp(symbol,_,_) -> symbol


  let rec eval singleStmt = (match singleStmt with
    | A.SeqStmt (stmt, stmtlist) -> (eval stmt; eval stmtlist)
    | A.SkipStmt(_) ->  ()
    | A.VarDeclStmt(_,sym,exp,_) ->(
      table := Symbol.insert sym exp !table;
      let value = compute (Symbol.lookup sym !table) table in
      Stack.push value stack;
      )
    | A.AssignStmt(lhs, rhs, _) ->(
      let rhs = compute (Symbol.lookup (matchLHS lhs rhs) !table) table in
      Stack.push rhs stack;
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
    | A.BlockStmt(statment, _) ->
        eval statment)
