module A = Ast;;

module type Temp = sig
  type temp
  val new_temp: unit
  type label
  val new_label: unit
end

module type TRANSLATE = sig
  type level
  type access
  val outermost : level

  val formals: access list
  val allocate_local: unit -> access
end;;

module type ENV = sig
  type enventry
end;;

module type FRAME = sig
  type frame
  type access
  val new_frame: string -> int list -> frame
  val allocate_local: frame -> access
  val allocate_temp: frame -> access
  type blob
end
