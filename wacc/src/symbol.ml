type symbol = string

(* convert the symbol type for a string *)
let symbol s = (s: symbol)
let name s = s
let string_of_symbol = name

module SymbolEqualSig = struct
  type t = symbol
  let compare (s1: t) (s2: t) = String.compare s1 s2
end

(* internally we use a binary search tree map for the symbol table *)
module ST = Map.Make(SymbolEqualSig)

(* table with itself and parent *)
type 'a table = Table of 'a ST.t * 'a table option

let empty = Table (ST.empty, None)

let parent = function
  | Table (t, Some parent) -> Some parent
  | Table (t, None) -> None

let new_scope (table: 'a table): 'a table = Table (ST.empty, Some table)

let rec lookup symbol = function
  | Table (table, Some parent) -> (
      try ST.find symbol table with
      | Not_found -> lookup symbol parent
    )
  | Table (table, None) -> ST.find symbol table

let rec lookup_opt (symbol: symbol) (t: 'a table): 'a option = match t with
  | Table (table, Some parent) -> (
      try let v = ST.find symbol table in
        Some (v)
      with
      | Not_found -> lookup_opt symbol parent
    )
  | Table (table, None) -> (
      try (let v = ST.find symbol table in
           (Some v))
      with
      | Not_found -> None
    )

let rec lookup_opt' (symbol: symbol) (t: 'a table): 'a option = match t with
  | Table (table, _) -> (
      try let v = ST.find symbol table in
        Some (v)
      with
      | Not_found -> None)

let lookup' symbol table =
  let Table (table', parent) = table in
  ST.find symbol table'

let insert symbol entry table =
  let Table (table', parent) = table in
  Table ((ST.add symbol entry table'), parent)
