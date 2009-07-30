
signature GNUPLOT =
sig

datatype style = Lines
               | Points
               | Steps
               | Impulses

datatype entry = E of {file: string,
                       cols: int * int,
                       title: string option,
                       style: style}
             
datatype input = I of {outfile: string option, (* postscript *)
                       title: string option,
                       entries: entry list,
                       xrange: (real * real) option,
                       yrange: (real * real) option}

val plot: input -> unit

(* Write tabular data to a file. *) 
val writeRealData: real list list * string -> unit
val writeIntData: int list list * string -> unit

end

