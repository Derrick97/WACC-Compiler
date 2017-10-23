module Temp = struct
  type temp
  type state
  type label

  val newtemp state -> unit -> temp
  val newlabel state -> unit -> label
  val namedlabel state -> string -> label

end
