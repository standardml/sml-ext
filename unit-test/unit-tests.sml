
structure UnitTests :> UNIT_TESTS =
struct 

structure List = ListExt
structure String = StringExt

open GeneralExt 

open PP.Ops 

type assertion = unit -> unit

type 'a assert_equal = 'a * 'a -> unit

datatype failure = GeneralFailure of string
                 | NotEqualFailure of string * string

exception Fail of failure

datatype test = TestCase of assertion
              | TestList of test list
              | TestLabel of string * test

(* 
Nodes are for determining the location of a failure.
*) 
datatype node  = ListItem of int
               | Label of string

(* 
A path is simply a list of nodes, from test case to root.
*) 
type path = node list

datatype options = Opt of { verbose: bool
                          , timeOut: Time.time option
                          }

fun nodeToString (Label s) = s
  | nodeToString (ListItem n) = Int.toString n

fun pathToString l = String.concat (List.separate ":" (map nodeToString (rev l)))

fun assertEqual c p (expected, actual) =
    if c (expected, actual) then () else
    let
       val expectedText = p expected
       val actualText = p actual
    in
       raise Fail (NotEqualFailure(expectedText, actualText))
    end

fun assertEqualEq ea = assertEqual op= (fn _ => "<no printer>") ea

fun assert true = ()
  | assert false = raise (Fail (GeneralFailure ""))

fun fail message = raise (Fail (GeneralFailure message))

val assertEqualReal = assertEqual Real.== Real.toString 

val assertEqualInt = assertEqual op= Int.toString 

fun testCount (TestCase _) = 1
  | testCount (TestList l) = foldr op+ 0 (map testCount l)
  | testCount (TestLabel(_,t)) = testCount t

fun ppFailure (GeneralFailure message) = %[$"Failure : ",$message]
  | ppFailure (NotEqualFailure (expected, actual)) =
    %[$"Failure : ",&[%[$"expected :<",$expected,$">"],
                      %[$"actual   :<",$actual,$">"]]]

val runTest' : options -> path -> test -> (int * int * int) * Time.time =
    let
       fun time f x =
           let
              val timer = Timer.startRealTimer()
              val result = f x
              val time = Timer.checkRealTimer timer
           in
              (result,time)
           end
       fun limit timeOut f x = 
           case timeOut of 
              SOME n => time (TimeLimit.timeLimit n f) x 
            | NONE => time f x
       fun run (options as Opt {timeOut, verbose}) path test =
           case test of 
              TestCase f => 
              let in 
                 let 
                    val (_, time) = limit timeOut f ()
                 in 
                    ((1,0,0), time)
                 end 
                 handle Fail failure => 
                        let in
                           PP.pp (&[%[$"### Failure in: ", $(pathToString path)],
                                    ppFailure failure])
                         ; ((1,1,0), Time.zeroTime)
                        end
                      | exn => 
                        let in
                           PP.pp (&[%[$"### Error in: ", $(pathToString path)],
                                    %[$"exception = ",$(exnName exn)]])
                         ; ((1,0,1), Time.zeroTime)
                        end
              end
            | TestList ts =>
              let
                 fun foldFn (i, test, ((total, failures, errors), time)) = 
                     let 
                        val ((t, f, e), m) = run options (ListItem i::path) test
                     in
                        ((total + t, failures + f, errors + e), Time.+(time, m))
                     end
              in           
                 List.foldli foldFn ((0,0,0), Time.zeroTime) ts
              end
            | TestLabel (name,t) =>
              let 
                 val indent = String.space ((length path div 2) * 3)
                 val _ = if not verbose 
                            then ()
                         else printl (indent ^ "[Running: " ^ name ^ "]")
                 val res as (_, time) = run options (Label name :: path) t
              in 
                 if not verbose 
                    then ()
                 else printl (indent ^ "[Done: " ^ name ^ " (" ^ Time.fmt 3 time ^ ")]")
               ; res
              end
    in
       run
    end


fun genTest options test = 
    let
       val ((t,f,e), time) = runTest' options [] test
       val fmt = &[~,
                   %[$"Cases    : ",PP.int (testCount test)],
                   %[$"Tried    : ",PP.int t],
                   %[$"Failures : ",PP.int f],
                   %[$"Errors   : ",PP.int e],
                   %[$"Time     : ",PP.time time],
                   ~]
    in
       PP.pp fmt
     ; {total = t,
        failures = f,
        errors = e}
    end 

val basic = Opt {verbose = false, timeOut = NONE}

val testResults = genTest basic
val runTest = ignore o genTest basic
val runVerbose = ignore o genTest (Opt {verbose = true, timeOut = NONE})

fun label (s,a) = TestLabel(s,TestCase a)

structure Ops =
   struct 
      val $ = TestLabel
      val % = TestCase
      val & = TestList
      val ? = assert
   end

end
