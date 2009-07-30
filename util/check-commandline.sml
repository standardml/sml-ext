
structure CheckCommandline:> CHECK_COMMANDLINE =
struct 

open GeneralExt

structure L = ListExt
structure S = StringExt

exception BadArgs of string

type aliases = (string * string) list
fun mkAliases x = map (fn {fullname,alias} => (fullname,alias)) x
val deAlias = L.assoc

type flags = string list
fun mkFlags x = x

datatype arg = Flag of string
             | Param of string * string
             | Undef of string

type args = arg list

datatype arg' = Flag' of string
              | Param' of string * string
              | Undef' of string
              | BadFlag' of string

fun arg'Arg (Flag' s) = Flag s
  | arg'Arg (Param' s) = Param s
  | arg'Arg (Undef' s) = Undef s
  | arg'Arg (BadFlag' _) = raise Fail "arg'Arg: BadFlag in arg list"

(* -------------------------------------------------------------------------- *)
(*  Parsing                                                                   *)
(* -------------------------------------------------------------------------- *)

local 
   structure P = Parsing
   infixr 4 << >>
   infixr 3 &&
   infix  2 -- ##
   infix  2 wth suchthat return guard when
   infixr 1 ||
   open P
in

(* begins with - *)
fun isLongArg s = S.isPrefix "--" s
fun isArg s = S.isPrefix "-" s

fun isntArg s = not (isArg s)

(* strip leading - *)
fun dearg s = if isLongArg s then S.substring(s,2,S.size s - 2)
              else if isArg s then S.substring(s,1,S.size s - 1)
              else raise Fail "dearg: not an arg"

(* if an '=' comes before a ' ' then change the '=' to space.  
                                                    e.g. if the argument is "-t=x=5", we want 't "x=5"'
 *)
fun split s n = [S.substring(s,0,n),S.substring(s,n+1,S.size s - n - 1)]

(* If there's an =, we have to return 2 args that were formerly 1. 
      So return a list *)
fun deEqual x = 
    case S.index x #"=" of 
       SOME n => 
       (case S.index x #" " of
           SOME m => if n < m (* = comes before ' ' *)
                     then split x n
                     else [x]
         | NONE => (* no spaces, so remove = *) 
           split x n)
     | NONE => [x] 

fun parseArgsAux flags arguments =
    let
       val flagParser: (arg',string) P.parser = 
           P.satisfy (fn x => isArg x andalso L.mem(dearg x,flags)) wth (Flag' o dearg)
       val paramParser: (arg',string) P.parser = 
           (P.satisfy isArg && P.satisfy isntArg) wth (fn (x,y) => Param'(dearg x,y))
       val undefParser: (arg',string) P.parser = 
           P.satisfy isntArg wth Undef'
       val badflagParser: (arg',string) P.parser = 
           P.satisfy isArg wth BadFlag'
       val argParser: (arg',string) P.parser = 
           flagParser || paramParser || undefParser || badflagParser
       val argsParser: (arg' list,string) P.parser = 
           P.repeat argParser -- P.done
       (* Get rid of the '=' sign *)                             
       val arguments' = L.concat (map deEqual arguments)
       val tokStream = Pos.markAny (Stream.fromList arguments')
       val args = P.parse argsParser tokStream 
    in
       case args of 
          NONE => NONE
        | SOME args => 
          case L.find (fn BadFlag' _ => true | _ => false) args of
             NONE => SOME (map arg'Arg args)
           | SOME (BadFlag' x) => raise BadArgs ("Unknown flag or parameter encountered: " ^ x)
           | _ => raise Impossible
    end

end (* local *) 

(* -------------------------------------------------------------------------- *)
(*  Simple interface                                                          *)
(* -------------------------------------------------------------------------- *)

fun dealiasArg aliases (arg as Param(k,v)) = 
    (case L.revAssoc(k,aliases) of
        SOME k' => Param(k',v)
      | NONE => arg)
  | dealiasArg aliases (arg as Flag f) =
    (case L.revAssoc(f,aliases) of
        SOME f' => Flag f'
      | NONE => arg)
  | dealiasArg _ (arg as Undef _) = arg

fun parseArgs (flags,aliases) arguments = 
    let
       (* add flag aliases to the flag list *)
       fun flagAlias (longname,alias) = if L.mem(longname,flags) 
                                        then SOME alias
                                        else NONE
       val flags' = flags @ L.mapPartial flagAlias aliases
       val (longs,shorts) = L.unzip aliases
       val names = longs @ shorts
       (* check that flag names are unique *)
       val _ = if L.unique flags' then () else raise BadArgs ("repeated flags: " ^ L.toString Fun.id flags')
       (* check that long names are unique and distinct from the (unique) short names *)
       val _ = if L.unique names then () else raise BadArgs ("repeated names: " ^ L.toString Fun.id names)
    in
       case parseArgsAux flags' arguments of
          SOME args => map (dealiasArg aliases) args
        | NONE => raise BadArgs "Couldn't parse command line"
    end

fun params xs = L.mapPartial (fn Param x => SOME x | _ => NONE) xs
fun flags xs = L.mapPartial (fn Flag x => SOME x | _ => NONE) xs
fun undefs xs = L.mapPartial (fn Undef x => SOME x | _ => NONE) xs

(* -------------------------------------------------------------------------- *)
(*  Checked Interface                                                         *)
(* -------------------------------------------------------------------------- *)

datatype kind = One
              | OneOrMore
              | ZeroOrOne
              | ZeroOrMore
                
fun optional One = false
  | optional OneOrMore = false
  | optional _ = true

fun required x = not (optional x)

type param = {name: string,
              kind: kind}

type checked_input = {flags: flags,
                      aliases: aliases,
                      params: param list,
                      undefs: int option}

type checked_args = {params: (string * string list) list,
                     flags: string list,
                     undefs: string list}

fun requiredParam ({params=inParams,...}:checked_input, {params,...}: checked_args,s) = 
    case L.find (fn {name,...} => s = name) inParams of
       SOME {kind,...} => 
       if required kind then
          case L.assoc (s,params) of
             SOME [h] => h
           | _ => raise Impossible
       else raise BadArgs ("not a required parameter: " ^ s)
     | NONE => raise BadArgs ("no such parameter: " ^ s)

fun optionalParam ({params=inParams,...}:checked_input, {params,...}: checked_args,s) = 
    case L.find (fn {name,...} => s = name) inParams of
       SOME {kind,...} =>
       if optional kind then
          case L.assoc (s,params) of
             SOME [h] => SOME h
           | NONE => NONE
           | _ => raise Impossible
       else raise BadArgs ("not an optional parameter: " ^ s)
     | NONE => raise BadArgs ("no such parameter: " ^ s)

fun requiredParams ({params=inParams,...}:checked_input, {params,...}: checked_args,s) = 
    case L.find (fn {name,...} => s = name) inParams of
       SOME {kind,...} => 
       if required kind then
          case L.assoc (s,params) of
             SOME l => l
           | _ => raise Impossible
       else raise BadArgs ("not a required parameter: " ^ s)
     | NONE => raise BadArgs ("no such parameter: " ^ s)

fun optionalParams ({params=inParams,...}:checked_input, {params,...}: checked_args,s) = 
    case L.find (fn {name,...} => s = name) inParams of
       SOME {kind,...} => 
       if optional kind then
          case L.assoc (s,params) of
             SOME l => l
           | NONE => []
       else raise BadArgs ("not an optional parameter: " ^ s)
     | NONE => raise BadArgs ("no such parameter: " ^ s)

fun checkedFlags ({flags,...}: checked_args) = flags

fun checkedUndefs ({undefs,...}: checked_args) = undefs

fun combineAux [] store = map (fn (x,xs) => (x,rev xs)) store
  | combineAux ((k,v)::t) store = 
    case L.assocRem(k,store) of
       NONE => combineAux t ((k,[v])::store)
     | SOME (vs,store') => combineAux t ((k,v::vs)::store')

fun combine xs = combineAux xs []

fun checkParam ps (name,values) =
    case L.find (fn x => #name x = name) ps of
       NONE => raise BadArgs ("Unknown parameter: " ^ name)
     | SOME {name,kind} => 
       case kind of
          One => if length values = 1 then ()
                 else raise BadArgs ("Expected exactly one parameter: " ^ name)
        | OneOrMore => if length values >= 1 then ()
                       else raise BadArgs ("Expected at least one parameter: " ^ name)
        | ZeroOrOne => if length values <= 1 then ()
                       else raise BadArgs ("Expected at most one parameter: " ^ name)
        | ZeroOrMore => ()

fun ensureParam ps {name,kind} =
    if optional kind then () else
    case L.assoc (name,ps) of
       SOME _ => ()
     | NONE => raise BadArgs ("Required argument is missing: " ^ name)
                     
fun checkArgs (input:checked_input) args = 
    let
       val undefs = L.mapPartial (fn Undef x => SOME x | _ => NONE) args
       val flags = L.mapPartial (fn Flag x => SOME x | _ => NONE) args
       val params = L.mapPartial (fn Param x => SOME x | _ => NONE) args
       val params = combine params
    in
       (case #undefs input of 
           SOME n => if length undefs = n then () else 
                     raise BadArgs ("wrong number of non-parameters: " ^ L.toString Fun.id undefs)
         | _ => ());
       app (checkParam (#params input)) params;
       app (ensureParam params) (#params input);
       if L.unique flags then () else raise BadArgs ("multiply defined flags: " ^ L.toString Fun.id flags);
       {params = params,
        flags = flags,
        undefs = undefs}
    end

fun parseCheckArgs input arguments =
    let
       val args = parseArgs (#flags input,#aliases input) arguments
    in
       checkArgs input args
    end
    
end
