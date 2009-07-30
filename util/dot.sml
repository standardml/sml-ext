
structure Dot =
struct 

(*** Misc ***)

type id = string

datatype directed = Directed | Undirected

datatype shape = PlainText
               | Circle
               | Box

type color = string

type fontname = string

type fontsize = int

(*** Nodes ***)

datatype node_style = Filled

datatype node_component = NodeLabel of string
                        | NodeShape of shape
                        | NodeColor of color
                        | NodeFillcolor of color
                        | NodeStyle of node_style
                        | NodeFontname of fontname
                        | NodeFontsize of fontsize

type node = id * node_component list

(*** Edges ***)

datatype edge_style = Dashed 
                    | Solid

datatype edge_component = EdgeLabel of string
                        | EdgeStyle of edge_style
                        | EdgeColor of color

type edge = id * id * edge_component list

(*** Graphs ***)

datatype graph_component = NodeDecl of node_component list
                         | Node of node
                         | EdgeDecl of edge_component list
                         | Edge of edge
                         | Subgraph of string option * graph_component list

datatype graph = Graph of string option * directed * graph_component list

(* -------------------------------------------------------------------------- *)
(*  Printing                                                                  *)
(* -------------------------------------------------------------------------- *)

open GeneralExt 
open PP.Ops

val pp : graph -> PP.pp = 
 fn Graph (name, dir, comps) =>
    let
       fun ppName NONE = PP.empty
         | ppName (SOME s) = $s
       fun ppDir Directed = $"digraph"
         | ppDir Undirected  = $"graph"
       fun attr (k, v) = %[k, $"=", PP.quote v]
       fun ppShape PlainText = $"plaintext"
         | ppShape Circle = $"circle"
         | ppShape Box = $"box"
       fun ppNodeStyle Filled = $"filled"
       fun ppColor s = $s
       fun ppNodeComp c = 
           case c of 
              NodeLabel s => attr($"label", $s)
            | NodeShape s => attr($"shape", ppShape s)
            | NodeColor s => attr($"color", ppColor s)
            | NodeFillcolor s => attr($"fillcolor", ppColor s)
            | NodeStyle s => attr($"style", ppNodeStyle s)
            | NodeFontname s => attr($"fontname", $s)
            | NodeFontsize s => attr($"fontsize", PP.int s)
       fun ppNodeComps cs = PP.brack(PP.separate ($", ") (map ppNodeComp cs))
       fun term p = %[p, $";"]
       fun ppNode (id, cs) = term(%[$id, \, ppNodeComps cs])
       fun ppEdgeStyle Dashed = $"dashed"
         | ppEdgeStyle Solid = $"solid"
       fun ppEdgeComp c = 
           case c of
              EdgeLabel s => attr($"label", $s)
            | EdgeStyle s => attr($"style", ppEdgeStyle s)
            | EdgeColor s => attr($"color", ppColor s)
       fun ppEdgeComps cs = PP.brack(PP.separate ($", ") (map ppEdgeComp cs))
       val arr = case dir of
                    Undirected => $" -- "
                  | Directed => $" -> "
       fun ppEdge (src, dest, comps) = 
           term(%[$src, arr, $dest, \, ppEdgeComps comps])
       fun ppGraphComp c = 
           case c of
              NodeDecl dec => term (%[$"node", \, ppNodeComps dec])
            | Node n => ppNode n
            | EdgeDecl dec => term(%[$"edge", \, ppEdgeComps dec])
            | Edge e => ppEdge e
            | Subgraph(name, comps) =>
              &[ %[$"subgraph", \, ppName name, \, $"{"]
               , %[\\, &(map ppGraphComp comps)]
               , $"}"
               ]
    in
       &[ ~
        , %[ppDir dir, \, ppName name, \, $"{"]
        , %[\\, &(map ppGraphComp comps)]
        , $"}"
        , ~]
    end

val test = Graph (SOME "Test", Directed, 
                  [ NodeDecl [NodeShape PlainText, NodeFontname "Courier", NodeFontsize 12]
                  , Subgraph (SOME "cluster1", 
                              [ Node ("0", [NodeLabel "A", NodeShape Circle, NodeFillcolor "grey43", NodeStyle Filled])
                              , Node ("1", [NodeLabel "B", NodeShape Circle, NodeFillcolor "aquamarine", NodeStyle Filled])
                              , Node ("2", [NodeLabel "C", NodeShape Circle, NodeFillcolor "pink", NodeStyle Filled])
                              , Edge ("0", "1", [EdgeLabel "AB", EdgeStyle Dashed, EdgeColor "orange"])
                              , Edge ("1", "2", [EdgeLabel "BC", EdgeStyle Dashed, EdgeColor "orange2"])
                              , Edge ("2", "0", [EdgeLabel "CA", EdgeStyle Dashed, EdgeColor "orange3"])
                              ])
                  , Subgraph (SOME "cluster2", 
                              [ Node ("3", [NodeLabel "D", NodeShape Circle, NodeFillcolor "yellow", NodeStyle Filled])
                              , Node ("4", [NodeLabel "E", NodeShape Circle, NodeFillcolor "blue", NodeStyle Filled])
                              , Node ("5", [NodeLabel "F", NodeShape Circle, NodeFillcolor "green", NodeStyle Filled])
                              , Edge ("3", "4", [EdgeLabel "DE", EdgeStyle Dashed, EdgeColor "red"])
                              , Edge ("4", "5", [EdgeLabel "EF", EdgeStyle Dashed, EdgeColor "red2"])
                              , Edge ("5", "3", [EdgeLabel "FD", EdgeStyle Dashed, EdgeColor "red3"])
                              ])
                  , Edge ("0", "5", [])
                  ])

val prepend : graph * string -> graph =
 fn (Graph(name, dir, cs), p) => 
    let
       fun prime s = p ^ s 
       fun primeGraphComponent c =
           case c of
              Node(id, cs) => Node(prime id, cs)
            | Edge(id1, id2, cs) => Edge(prime id1, prime id2, cs)
            | Subgraph(name, gs) => Subgraph(name, map primeGraphComponent gs)
            | _ => c
    in
       Graph(name, dir, map primeGraphComponent cs)
    end

val extend : graph * graph_component list -> graph =
 fn (Graph(name, dir, cs), cs') => Graph(name, dir, cs @ cs')

end
