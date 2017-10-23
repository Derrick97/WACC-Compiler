type symbol
type 'a table

val symbol : string -> symbol
val name : symbol -> string

(* alias for symbol *)
val string_of_symbol : symbol -> string
(* create an empty symbol table *)
val empty  : 'a table

(* create a child table with parent set to table *)
val new_scope : 'a table -> 'a table

(* get the parent table of the current table *)
val parent : 'a table -> 'a table option

(* insert a binding into the table *)
val insert : symbol -> 'a -> 'a table -> 'a table

(* lookup a particular symbol from the table,
   will raise Not_found if the symbol is not found in all scope *)
val lookup : symbol -> 'a table -> 'a

(* lookup a particular symbol from the table IN CURRENT SCOPE
   will raise Not_found if the symbol is not found in all scope *)
val lookup' : symbol -> 'a table -> 'a
