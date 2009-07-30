
(**
 * positions within a file (for reporting errors) 
 * 
 * when reading a position, it is always returned as a pair
 * of the left edge and right edge, since a position may
 * delimit a range. If a point, then these are equal. 
 *
 * if positions are ranges (start,finish):          
 *   union ((s,f),(s',f')) = (min (s,s'),max (f,f')) 
 *   max   ((s,f),(s',f')) = (max (s,s'),max (f,f')) 
 *   min   ((s,f),(s',f')) = (min (s,s'),min (f,f')) 
*)
signature POS =
sig

type pos

val initPos: pos
val min: pos * pos -> pos
val max: pos * pos -> pos
val union: pos * pos -> pos
val toString: pos -> string

val markStream: char Stream.stream -> (char * pos) Stream.stream

(* alternately, pass in a filename *)
val markFileStream: string -> char Stream.stream -> (char * pos) Stream.stream

val markAny: 'a Stream.stream -> ('a * pos) Stream.stream
val initFilePos: string -> pos
val nextChar: pos -> pos
val nextLine: pos -> pos
val rightEdge: pos -> pos

(* absolute source file position *)
val getAbs: pos -> int * int

val getCol: pos -> int * int

(* Line of start position *)
val getLine: pos -> int

end
