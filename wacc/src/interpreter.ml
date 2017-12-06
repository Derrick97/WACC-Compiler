open Ast_v2
module A = Ast_v2

exception Divide_By_Zero

type wrapType =
  | Int of int
  | Char of char
  | Bool of bool
  | String of string
  | NullType
  | ListOfElem of int * A.exp list
  | PairsTy of A.exp * A.exp * bool
  | DefaultTy
  | ErrorTy

let func_table: 'a Symbol.table ref = ref (Symbol.empty)

let function_return: wrapType ref = ref (DefaultTy)

let stack: wrapType Stack.t = (Stack.create ())
(*let table = ref (Symbol.empty)*)

let rec add_func_dec decs =
   match decs with
   | [] -> ()
   | h::r -> begin
      let (A.FuncDec(ty, ident, field_list, stmt),pos) = h in
      func_table := Symbol.insert ident (ty, field_list, stmt) !func_table;
      add_func_dec r
   end


let replaceList index elems newItem = (
  let finalList = [] in
  for i = 0 to (List.length elems)-1 do
    if(i = index) then newItem::finalList
    else finalList::(List.nth elems i)
  done
)

let rec eq_wrapTy ty1 ty2 =
  match (ty1,ty2) with
  | NullType,  NullType -> true
  (*| PairsTy (fst,snd), PairsTy (fst',snd') -> eq_wrapTy fst fst' && eq_wrapTy snd snd'*)
  | Int(i), Int(j) -> i==j
  | Char(a), Char(b) -> Char.compare a b == 0
  | String (str1), String (str2) -> String.compare str1 str2 == 0
  | Bool (a), Bool (b) ->  (a && b) || ((not a) && (not b))
  | _ , DefaultTy -> true
  | DefaultTy, _ -> true
  | _ -> false


let rec print_char_array elems =
  match elems with
  | [LiteralExp(LitChar(chr)),pos] -> print_char chr
  | (LiteralExp(LitChar(chr)),pos)::r -> print_char chr; print_char_array r
  | _ -> assert false

let printWrap wrapTy = (
  match wrapTy with
  | Int(num) -> print_int num
  | Char(letter) -> print_char letter
  | Bool(tf) -> if tf then print_string "true" else print_string "false"
  | String(str) -> print_string str
  | NullType -> print_string "(nil)"
  | ListOfElem(length,elems) ->(
    match List.hd elems with
    | LiteralExp(LitChar(chr)), pos -> print_char_array elems
    | _ ->  print_string "0x12008")
  | PairsTy(_,_,_) -> print_string "0x22008"
  | DefaultTy -> print_string "Will Fix"
  | ErrorTy -> print_string "ERROR CODE")

let arithmetic_convert wrapTy =
  match wrapTy with
  | Int(num) -> num
  | Char (chr) -> Char.code chr
  | NullType
  | _ -> -1                   (*TODO:Need Revision here to make it throw an exception*)

let booleanConvert wrapTy =
  match wrapTy with
  | Bool(tf) -> tf
  | _ -> false

  (*Test Code*)
let rec compute exp table =
let (exp', pos) = exp in
match exp' with
| A.IdentExp (symbol) -> Symbol.lookup symbol !table
| A.NullExp -> NullType
| A.LiteralExp(literal) -> (match literal with
  | A.LitString(str) -> String(str)
  | A.LitBool(boo) -> Bool(boo)
  | A.LitChar(cha) -> Char(cha)
  | A.LitInt (num) -> Int(num)
  | A.LitArray (expList) -> (
    let length = List.length expList in
    ListOfElem(length, expList);
    )
  | A.LitPair(fst, snd) -> PairsTy(fst,snd,true)
  | A.LitNull -> NullType)
| A.BinOpExp (ex1, binop, ex2) -> (
  let lhs = arithmetic_convert (compute ex1 table) in
  let rhs = arithmetic_convert (compute ex2 table) in
  match binop with
  | A.PlusOp -> Int(lhs + rhs)
  | A.MinusOp -> Int(lhs - rhs)
  | A.TimesOp -> Int(lhs * rhs)
  | A.DivideOp -> begin
       if rhs == 0 then let () = print_endline "RuntimeError: Divide_By_Zero" in exit 255
       else
       Int(lhs / rhs)
    end
  | A.GeOp -> Bool (lhs >= rhs)
  | A.GtOp -> Bool (lhs > rhs)
  | A.EqOp ->  Bool (eq_wrapTy (compute ex1 table) (compute ex2 table))
  | A.NeOp -> Bool (not (eq_wrapTy (compute ex1 table) (compute ex2 table)))
  | A.LtOp -> Bool (lhs < rhs)
  | A.LeOp -> Bool (lhs <= rhs)
  | A.AndOp -> Bool (booleanConvert (compute ex1 table) && booleanConvert(compute ex2 table))
  | A.OrOp -> Bool (booleanConvert (compute ex1 table) || booleanConvert(compute ex2 table))
  | A.ModOp -> Int(lhs mod rhs))
| A.UnOpExp (unop, exp) -> (
  let operand = compute exp table in
  match unop with
  | A.NotOp -> Bool(not(booleanConvert operand));
  | A.NegOp -> Int(0-(arithmetic_convert operand));
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
| A.ArrayIndexExp(symbol, explist) -> ( match (Symbol.lookup symbol !table) with
  | ListOfElem (length, elems) -> indexList (ListOfElem(length, elems)) explist table
  | _ -> ErrorTy)
| A.NewPairExp (ex1,ex2) ->  PairsTy(ex1, ex2,true)
| A.FstExp (exp) -> (match compute exp table with
  | PairsTy (ex1,ex2, true) ->  compute ex1 table
  | _ -> ErrorTy)
| A.SndExp (exp) -> (match compute exp table with
  | PairsTy (ex1,ex2, true) ->  compute ex2 table
  | _ -> ErrorTy)
| A.CallExp (ident, explist) -> begin
    let (ty, field_list,stmt) = Symbol.lookup ident !func_table in
    let func_scope = ref (Symbol.new_scope !table) in
    let () = match (field_list, explist) with
    | [],[] -> eval stmt func_scope
    | (h::r,h'::r') -> List.map2 (fun x y -> begin
      let (ty', ident') = x in
      eval (VarDeclStmt (ty',ident',y),pos) func_scope end) field_list explist; eval stmt func_scope
    in !function_return
  end


and indexList (ListOfElem(length,elems)) indexExps table =
  match indexExps with
  | index::[] -> begin
    let index_num = arithmetic_convert (compute index table) in
    if index_num < 0 || index_num > length - 1 then let () = print_endline "RuntimeError: Index out of array bound" in exit 255
    else
    compute (List.nth elems (arithmetic_convert (compute index table))) table
    end
  | h::r -> begin
    let index_num = arithmetic_convert (compute h table) in
    if index_num < 0 || index_num > length - 1 then let () = print_endline "RuntimeError: Index out of array bound" in exit 255
    else
    indexList (compute (List.nth elems (arithmetic_convert (compute h table))) table) r table
    end

and print_pair (PairsTy (ex1,ex2,still_valid)) table = begin ((*Only for debugging use*)
    print_string "Pair (";
    printWrap (compute ex1 table);
    print_string ",";
    printWrap (compute ex2 table);
    print_endline ")";) end

and modify_array elems indexExps new_value table =
    match indexExps with
    | [] -> []
    | [exp] -> begin
      let (Int(num)) = compute exp table in
      let buffer_list = ref [] in
      for i = 0 to (List.length elems - 1) do
         if i != num then
          buffer_list := !buffer_list @ [List.nth elems i]
         else
          buffer_list := !buffer_list @ [new_value]
      done;
      !buffer_list;
    end
    | _ -> assert false (*TODO: Multidimension modifications*)

and matchLHS lhs rhs table =
  let (lhs', pos) = lhs in
  match lhs' with
  | A.IdentExp(symbol) -> symbol
  | A.FstExp(exp) -> (
    let (exp', pos) = exp in
    match exp' with
    | A.IdentExp(symbol) -> (
      let res = compute exp table in
      match res with
      | PairsTy(ex1,ex2,true) -> print_pair res table; table := Symbol.insert symbol (PairsTy(rhs,ex2,true)) (!table); symbol;
      | _ -> assert false (*Need to throw errors*)
      )
    | A.ArrayIndexExp(symbol,explist) -> (
      let elem_list = Symbol.lookup symbol !table in
      let value = indexList elem_list explist table in
      table := Symbol.insert symbol value (!table);
      symbol;
      )
    | A.FstExp(exps) -> matchLHS exps rhs table
    | A.SndExp(exps) -> matchLHS exps rhs table
    | A.NullExp -> (
      table := Symbol.insert "nil" (NullType) (!table);
      Symbol.symbol "nil";)
    | _ -> Symbol.symbol "what"
    )
  | A.SndExp(exp) -> (
    let (exp', pos) = exp in
    match exp' with
    | A.IdentExp(symbol) -> (let res = compute exp table in
      match res with
      | PairsTy(ex1,ex2,true) -> print_pair res table; table := Symbol.insert symbol (PairsTy(ex1,rhs,true)) (!table); symbol;
      | _ -> assert false (*Need to throw errors*)
    )
    | A.ArrayIndexExp(symbol,_) -> ("snd[]" ^ symbol)
    | A.FstExp(exps) -> matchLHS exps rhs table
    | A.SndExp(exps) -> matchLHS exps rhs table
    | A.NullExp -> (
      table := Symbol.insert "nil" (NullType) (!table);
      Symbol.symbol "nil";)
    | _ -> assert false)
  | A.ArrayIndexExp(symbol,explist) -> (
    let res = Symbol.lookup symbol !table in
    let (ListOfElem (length, elem_list), is_array) = match res with
    | ListOfElem (length, elem_list) -> (ListOfElem (length, elem_list),true)
    | String (str) -> (string_to_array str pos, false)
    | _ -> assert false in
    let index = List.nth explist 0 in
    let new_list = modify_array elem_list [index] rhs table in
    table := Symbol.insert symbol (ListOfElem(length, new_list)) (!table);
    symbol;
    )

and string_to_array str pos =
    let length = String.length str in
    let buffer_list = ref [] in
    for i = 0 to length -1 do
      buffer_list := !buffer_list @ [A.LiteralExp(LitChar(String.get str i)),pos]
    done ;
    ListOfElem(length,!buffer_list)

and eval singleStmt table = (
    let (singleStmt', pos) = singleStmt in
    match singleStmt' with
    | A.SeqStmt (stmt, stmtlist) -> (eval stmt table; eval stmtlist table;)
    | A.SkipStmt ->  ()
    | A.VarDeclStmt(_,sym,exp) ->(
      let value = compute exp table in
      table := Symbol.insert sym value !table;
      Stack.push value stack;
      )
    | A.AssignStmt((A.FstExp exp,pos), rhs) ->(
      let value = compute rhs table in
      matchLHS (A.FstExp exp, pos) rhs table;
      Stack.push value stack;
      )
    | A.AssignStmt((A.SndExp exp,pos), rhs) ->(
      let value = compute rhs table in
      matchLHS (A.SndExp exp, pos) rhs table ;
      Stack.push value stack;
      )
    | A.AssignStmt((A.ArrayIndexExp(sym,expList),pos), rhs) ->(
      let value = compute rhs table in
      matchLHS (A.ArrayIndexExp (sym,expList), pos) rhs table ;
      Stack.push value stack;
      )
    | A.AssignStmt(lhs, rhs) ->(
      let value = compute rhs table in
      table := Symbol.insert (matchLHS lhs rhs table) value !table;
      Stack.push value stack;
      )
    | A.FreeStmt(exp) -> ((*
      let value = compute exp table in
      let (PairsTy(ex1,ex2,still_valid)) = value in
      if still_valid then let () = still_valid = false in ()
      else let () = print_newline "RuntimeError: Cannot free a variable that can be already freed." in exit 255
      )*))
    | A.PrintStmt(newline,exp) -> (
        Stack.push (compute exp table) stack;
        let result = Stack.pop stack in
        printWrap result;
        if newline then print_newline();
        )
    | A.ReadStmt(exp) -> (
      (*matchLHS exp exp is a hack here, because I only expect the expression to be identExp, but I'm not sure.*)
      (*If only aims to pass test, then the exp can only be identExp*)
        let value = compute exp table in
        match value with
        | Int(_) -> begin
          let num = read_int () in
          let symbol = matchLHS exp exp table in
          table := Symbol.insert symbol (Int(num)) !table;
          Stack.push value stack;
          end
        | Char(_) -> begin
          let line = read_line () in
          let chr = String.get line 0 in
          let symbol = matchLHS exp exp table in
          table := Symbol.insert symbol (Char(chr)) !table;
          Stack.push value stack;
          end
        | String(_) -> begin
          let line = read_line () in
          let symbol = matchLHS exp exp table in
          table := Symbol.insert symbol (String(line)) !table;
          Stack.push value stack;
          end
        | _ -> assert false
      )
    | A.WhileStmt(exp, stmt) -> (
        let cond = compute exp table in
          match cond with
          | Bool(true) -> (eval stmt table; eval (A.WhileStmt(exp ,stmt), pos) table;)
          | Bool(false) -> ();
          | _ -> print_string ("This is not a boolean expression, cannot used for condition.");
      )
    | A.IfStmt(exp,stmt1,stmt2) -> (
      let cond = compute exp table in
        match cond with
        | Bool(true) -> eval stmt1 table
        | Bool(false) -> eval stmt2 table
        | _ -> print_string ("This is not a boolean expression, cannot used for condition.");
      )
    | A.ExitStmt (exp) ->(
      let exit_code = compute exp table in
         match exit_code with
         | Int(i) -> begin
           if i < 255 && i>=0 then exit i
           else if i < 0 then exit (i+256)
           else exit (256 mod i)
         end
         | _ -> assert false
      )
    | A.RetStmt (exp) -> begin
      let value = compute exp table in
      function_return := value
    end
    | A.BlockStmt(statment) ->
        let new_table = Symbol.new_scope (!table) in
        let new_table_ref = ref new_table in
        eval statment new_table_ref )
