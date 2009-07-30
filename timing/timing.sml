
(* Author: Frank Pfenning, modified Sean McLaughlin *)

structure Timing :> TIMING =
struct

open PP.Ops

(* 
 user and system time add up to total CPU time used 
 gc time is a portion of the total CPU time devoted to garbage collection 
 *)
datatype cpuTime = Cpu of { usr: Time.time
                          , sys: Time.time
                          , gc: Time.time
                          }

type realTime = Time.time

fun init () = ()

datatype 'a result = Value of 'a
                   | Exception of exn

type center = string * (cpuTime * realTime) ref

type sum = string * center list

val zero = Cpu { usr = Time.zeroTime
               , sys = Time.zeroTime
               , gc = Time.zeroTime
               }

(* 
 fun minus ({usr = t1, sys = t2, gc = t3},
 {usr = s1, sys = s2, gc = s3}) =
 {usr = Time.-(t1,s1), sys = Time.-(t2,s2), gc = Time.-(t3,s3)}
 *) 

fun plus (Cpu {usr, sys, gc},
	  Cpu {usr = usr', sys = sys', gc = gc'}) =
    Cpu { usr = Time.+(usr,usr')
        , sys = Time.+(sys,sys')
        , gc = Time.+(gc,gc')
        }

fun sum (Cpu {usr, sys, ...}) = Time.+ (usr, sys)

fun newCenter (name) = (name, ref (zero, Time.zeroTime))

fun reset (_, counters) = counters := (zero, Time.zeroTime)

(* time center f x = y
 runs f on x and adds its time to center.
 If f x raises an exception, this is properly re-raised

 Warning: if the execution of f uses its own centers,
 the time for those will be counted twice!
 *)
fun checkCPUAndGCTimer timer =
    let
       val {usr = usr, sys = sys} = Timer.checkCPUTimer timer
       val gc = Timer.checkGCTime timer
    in
       Cpu {usr = usr, sys = sys, gc = gc}
    end

fun time (_, counters) (f:'a -> 'b) (x:'a) =
    let
       val realTimer = Timer.startRealTimer ()
       val CPUTimer = Timer.startCPUTimer ()
       val result = Value (f x) handle exn => Exception (exn)
       val evalCPUTime = checkCPUAndGCTimer (CPUTimer)
       val evalRealTime = Timer.checkRealTimer (realTimer)
       val (CPUTime, realTime) = !counters
       val _ = counters := (plus (CPUTime, evalCPUTime),
			    Time.+ (realTime, evalRealTime))
    in
       case result
	of Value (v) => v
	 | Exception (e) => raise e
    end

(* 
 sumCenter (name, centers) = sc
 where sc is a new sum which contains the sum of the timings of centers.

 Warning: the centers should not overlap!
 *)
fun sumCenter (name, l) = (name, l)

(* -------------------------------------------------------------------------- *)
(*  Printing                                                                  *)
(* -------------------------------------------------------------------------- *)

(* 
[Total]
  Real  : 0.006
  Run   : 0.006 (usr : 0.005, sys : 0.005, gc : 0.005)
*) 
fun pp' (name, (CPUTime as Cpu {usr, sys, gc}, realTime)) =
    &[ PP.brack($name)
     , %[\\, &[ %[$"Real  : ", PP.time realTime]
              , %[$"Run   : ", PP.time (sum CPUTime), $" (usr : ", PP.time usr, $", sys : ", PP.time sys, $", gc  : ", PP.time gc, $")"]
              ]
        ]
     ]

fun pp (name, ref times) = pp'(name, times)

fun ppSum (name, centers) = 
    let
       fun sumup (nil, (CPUTime, realTime)) = pp' (name, (CPUTime, realTime))
	 | sumup ((_, ref (C, R))::centers, (CPUTime, realTime)) =
	   sumup (centers, (plus (CPUTime, C), Time.+ (realTime, R))) 
    in 
       sumup (centers, (zero, Time.zeroTime))
    end

end
