(** A symbol table with parents**)
type symbol = String.t
type 'a table

val symbol : string -> symbol

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

val lookup_opt  : symbol -> 'a table -> 'a option
val lookup_opt' : symbol -> 'a table -> 'a option
(* lookup a particular symbol from the table IN CURRENT SCOPE
   will raise Not_found if the symbol is not found in all scope *)
val lookup' : symbol -> 'a table -> 'a
val iter_local: 'a table -> ('a -> unit) -> unit
(* val map_local: 'a table -> ('a -> 'b) ->  'b table *)
