
structure Gnuplot:> GNUPLOT =
struct 

structure L = ListExt
structure T = TextIOExt

nonfix $ % & \ \\ ~
open PP.Ops                   
open GeneralExt 

datatype style = Lines
               | Points
               | Steps
               | Impulses

datatype entry = E of {file: string,
                       cols: int * int,
                       title: string option,
                       style: style}
             
datatype input = I of {title: string option,
                       entries: entry list,
                       xrange: (real * real) option,
                       yrange: (real * real) option}

val tmpDir = "/tmp"

fun ppStyle Lines = $"with lines"
  | ppStyle Points = $"with points"
  | ppStyle Steps = $"with steps"
  | ppStyle Impulses = $"with impulses"

fun ppCols (n, m) = %[$"using ", PP.int n, $":", PP.int m]

fun ppTitle NONE = PP.empty
  | ppTitle (SOME s) = %[$"title ", PP.quote ($s)]

fun ppFile s = PP.quote s

fun ppEntry (E {file, cols, title, style}) =
    %[ppFile file, \, ppCols cols, \, ppTitle title, \, ppStyle style]

fun ppPlot entries = 
    let
       fun pp1 entry = %[ppEntry entry, $", \\"]
    in
       %[$"plot ",&(L.mapButlast (pp1, ppEntry) entries)]
    end

fun ppInput (I {title, entries, xrange, yrange}) =
    &[ppPlot entries,
      ~,
      $"set term postscript",
      ~,
      $"set output 




end

