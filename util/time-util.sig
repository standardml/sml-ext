
signature TIME_UTIL =
sig

type dhms = {days: IntInf.int,
             hours: IntInf.int,
             minutes: IntInf.int,
             seconds: IntInf.int}
(** dhms ("Days, Hours, Minutes, Seconds") *)

val dhmsFromInts: {days: int, 
                   hours: int, 
                   minutes: int, 
                   seconds: int} -> dhms

val dhmsToString: dhms -> string
val secondsToDhms: IntInf.int -> dhms
val dhmsToSeconds: dhms -> IntInf.int
val dhmsFromString: string -> dhms option
(* dhms_from_string "1:2:3:4" ~~> {days = 1, hours 2 = ...} *)

val secondsToMinutes: IntInf.int -> real
val secondsToHours: IntInf.int -> real
val secondsToDays: IntInf.int -> real
val currentDate: unit -> Date.date
val time: ('a -> 'b) -> 'a -> 'b * Time.time
(** Timed apply *)

val microsecsPerCall: ('a -> 'b) * 'a * IntInf.int -> IntInf.int

type long_computation = {time: Time.time,
                         startDate: Date.date,
                         endDate: Date.date}

val mkLongComputation: ('a -> 'b) -> 'a -> 'b * long_computation

end
