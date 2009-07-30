
signature UNIT_TESTS =
sig

type assertion = unit -> unit

(* expected actual *)
type 'a assert_equal = 'a * 'a -> unit

datatype failure = GeneralFailure of string
                 | NotEqualFailure of string * string

datatype test = TestCase of assertion
              | TestList of test list
              | TestLabel of string * test

exception Fail of failure

datatype options = Opt of { verbose: bool
                          , timeOut: Time.time option
                          }

val assertEqual: ('a * 'a -> bool) -> ('a -> string) -> 'a assert_equal
val assertEqualEq: ''a assert_equal
val assert: bool -> unit
val fail: string -> unit
val assertEqualReal: real assert_equal
val assertEqualInt: int assert_equal
val label: string * assertion -> test
val testCount: test -> int
val runTest: test -> unit
val runVerbose: test -> unit
val genTest: options -> test -> {total: int,
                                 failures: int,
                                 errors: int}
val testResults: test -> {total: int,
                          failures: int,
                          errors: int}

structure Ops :
             sig
                val $ : string * test -> test (* TestLabel *)
                val % : assertion -> test (* TestCase *)
                val & : test list -> test (* TestList *)
                val ? : bool -> unit (* assert *)
             end
end


