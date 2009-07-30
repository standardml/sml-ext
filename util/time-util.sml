
structure TimeUtil:> TIME_UTIL =
struct 

structure I = IntInf

type dhms = {days: I.int,
             hours: I.int,
             minutes: I.int,
             seconds: I.int}

fun real x = IntExt.infToReal x

fun secondsToMinutes x = real x / 60.0

fun secondsToHours x = real x / 3600.0

fun secondsToDays x = real x / 86400.0

fun secondsToDhms (x: I.int) = 
    let
       val days = x div 86400
       val x = x - days * 86400
       val hours = x div 3600
       val x = x - hours * 3600
       val minutes = x div 60
       val x = x - minutes * 60
    in
       {days = days,
        hours = hours,
        minutes = minutes,
        seconds = x}
    end

fun dhmsFromInts {days,hours,minutes,seconds} = 
    {days = IntInf.fromInt days,
     hours = IntInf.fromInt hours,
     minutes = IntInf.fromInt minutes,
     seconds = IntInf.fromInt seconds}

fun dhmsToSeconds ({days,hours,minutes,seconds}: dhms) = 
    86400 * days + 3600 * hours + 60 * minutes + seconds

fun dhmsToString {days,hours,minutes,seconds} =
    concat [I.toString days," days, ",I.toString hours," hours, ",
            I.toString minutes," minutes, ",I.toString seconds," seconds"]

fun currentDate () = Date.fromTimeLocal (Time.now())

type long_computation = {time: Time.time,
                         startDate: Date.date,
                         endDate: Date.date}

fun mkLongComputation f x =
    let
       val timer = Timer.startRealTimer()
       val startDate = currentDate()
       val result = f x
       val time = Timer.checkRealTimer timer
       val endDate = currentDate()
    in
       (result,{time = time,
                startDate = startDate,
                endDate = endDate})
    end

fun time f x =
    let
       val timer = Timer.startRealTimer()
       val result = f x
       val time = Timer.checkRealTimer timer
    in
       (result,time)
    end

fun microsecsPerCall (f,x,n) = 
    let
       val timer = Timer.startRealTimer()
       fun itFn 0 = ()
         | itFn n = (ignore (f x); itFn (n-1))
       val _ = itFn n
       val elapsedTime = Timer.checkRealTimer timer
       val microsecs = Time.toMicroseconds elapsedTime
    in
       microsecs div n
    end

structure P = Parsing
structure S = Stream

local 
   infixr 4 << >>
   infixr 3 &&
   infix  2 -- ##
   infix  2 wth suchthat return guard when
   infixr 1 ||

   open P
in

val ints = [#"0",#"1",#"2",#"3",#"4",#"5",#"6",#"7",#"8",#"9"]

val intParser : (Int.int,char) P.parser = 
    P.repeat1 (P.oneof ints) wth (valOf o Int.fromString o implode)

val colonParser : (unit,char) P.parser = P.literal #":" wth ignore

fun parsep s = Pos.markAny (S.fromList (explode s))

val dhmsParser : (dhms,char) P.parser = 
    ((intParser << colonParser) && (intParser << colonParser)) &&
     ((intParser << colonParser) && intParser) wth 
     (fn ((days,hours),(minutes,seconds)) => 
         dhmsFromInts {days = days,
                       hours = hours,
                       minutes = minutes,
                       seconds = seconds})

fun dhmsFromString s = 
    P.parse dhmsParser (parsep s)

end (* local *)

end
