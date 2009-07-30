
signature TEXT_IO_EXT =
sig

include TEXT_IO

val printl : string -> unit
val outputl : TextIO.outstream -> string -> unit

val readFile : string -> string
(** 
 * @exception IO.Io if file doesn't exist
 *)

val writeFile : {file: string, s: string} -> unit
val appendFile : {file: string, s: string} -> unit

end
