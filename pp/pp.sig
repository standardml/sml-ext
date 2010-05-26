

(**
 * See http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
 * and http://caml.inria.fr/resources/doc/guides/format.html
 * for usage.
 *)

signature PP =
sig

structure P : PP_DESC
structure PP : PP_STREAM

type stream

val outStream: TextIO.outstream -> int -> stream
val stdoutStream: int -> stream

type pp

val hbox: pp list -> pp
val vbox: pp list -> pp
val vBox: int -> pp list -> pp
val vboxNewline: pp list -> pp
val vboxNewline2: pp list -> pp
val hvBox: int -> pp list -> pp
val hvbox: pp list -> pp
val hovBox: int -> pp list -> pp
val hovbox: pp list -> pp
val string: string -> pp
val char: char -> pp
val space: int -> pp
val cut: pp
val newline: pp
val nbspace: int -> pp
val empty: pp 
val cutl: pp list -> pp 
val cutl2: pp list -> pp 
val quote: pp -> pp (* "p" *)
val paren: pp -> pp (* (p) *)
val brack: pp -> pp (* [p] *)
val curly: pp -> pp (* {p} *)
val pair: pp * pp -> pp (* (p1,p2) *)
val bool: bool -> pp
val int: int -> pp
val intinf: IntInf.int -> pp
val real: real -> pp
val realFmt: int -> real -> pp
val time: Time.time -> pp
val date: Date.date -> pp
val separate: pp -> pp list -> pp
val unit: pp
val commas: ('a -> pp) -> 'a list -> pp
val listVert: ('a -> pp) -> 'a list -> pp
val listHoriz: ('a -> pp) -> 'a list -> pp
val setVert: ('a -> pp) -> 'a list -> pp
val setHoriz: ('a -> pp) -> 'a list -> pp
val arrayVert: ('a -> pp) -> 'a array -> pp
val arrayHoriz: ('a -> pp) -> 'a array -> pp
val array2: ('a -> pp) -> 'a Array2.array -> pp
val vectorVert: ('a -> pp) -> 'a vector -> pp
val vectorHoriz: ('a -> pp) -> 'a vector -> pp
val tupleHoriz: ('a -> pp) -> 'a list -> pp (* (p1,p2,...,pn) *)
val tupleVert: ('a -> pp) -> 'a list -> pp (* (p1,p2,...,pn) *)
val option: ('a -> pp) -> 'a option -> pp

val banner: {length: int, 
             lhs: string,
             rhs: string,
             char: char} -> string -> pp

val stripe: {length: int, 
             lhs: string,
             rhs: string,
             char: char} -> pp

(* Put in pp spaces for each space, with 2 following a period. *)
val text: string -> pp 

(* put PP spaces in place of all the ascii spaces in a string. *)
val tokenize: string -> pp list

(* write and flush the stream *)
val doit: stream -> pp -> unit
val writeFileColumn: string * int * pp -> unit
val writeFile: string * pp -> unit

(* print to stdout, with 80 char width *)
val pp: pp -> unit
val ppl: pp -> unit

structure Ops:
             sig
                val $ : string -> pp (* string *)
                val $$ : string -> pp (* text *)
                val % : pp list -> pp (* hbox *)
                val %% : pp list -> pp (* box *)
                val & : pp list -> pp (* vboxNewline *)
                val && : pp list -> pp (* vboxNewline2 *)
                val // : pp (* cut *)
                val ~ : pp (* empty *)
                val \ : pp (* space 1 *)
                val \\ : pp (* nbspace 2 *)
             end

end

