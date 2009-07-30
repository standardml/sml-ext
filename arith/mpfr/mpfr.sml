
structure MPFR :> MPFR =
struct 

structure A = Array
structure F = MLton.Finalizable
structure P = MLton.Pointer
structure S = StringExt
open GeneralExt

type ptr = P.t 
type t' = ptr
type t = t' F.t

exception MPFR

(* -------------------------------------------------------------------------- *)
(*  Util                                                                      *)
(* -------------------------------------------------------------------------- *)

fun withval1 f x = F.withValue(x,f)
fun withval2 f (x1, x2) = F.withValue(x1, fn x1 => F.withValue (x2, fn x2 => f(x1, x2)))
fun withval3 f (x1, x2, x3) = F.withValue(x1, fn x1 => 
                              F.withValue(x2, fn x2 => 
                              F.withValue(x3, fn x3 => f(x1, x2, x3))))

(* Make a string on the MLton side *) 
val mk_str = _export "mlton_mk_str": (int -> char array) -> unit;
fun mk_fn n = A.tabulate(n, fn _ => #"X")
val _ = mk_str mk_fn

(* -------------------------------------------------------------------------- *)
(*  Interface                                                                 *)
(* -------------------------------------------------------------------------- *)

fun toString x = 
    let
       val to_string = _import "mlton_mpfr_to_string" : t' -> char array;
       val s = withval1 to_string x
       val s = A.vector s
       val s = S.truncateCString s
    in
       s
    end

fun fromPtr p = 
    let 
       val z = F.new p
       val free = _import "mlton_mpfr_free" : t' -> unit;
    in 
       F.addFinalizer(z, free)
     ; z
    end

val asPtr = F.withValue

fun new () = 
    let
       val new = _import "mlton_mpfr_new" : unit -> t';
       val x = new()
    in
       fromPtr x
    end

fun fromString s =
    let
       val x = new()
       val mpfr_set_str = _import "mlton_mpfr_set_str" : t' * string -> int;
       val s' = S.nullTerminate s
       val n = F.withValue(x, fn x => mpfr_set_str(x, s'))
    in
       if n = 0 then x else raise MPFR
    end       

val fromInt = fromString o Int.toString

val zero = fromString "0.0"

val one = fromString "1.0"

(* MPFR return value is unused for arithmetic functions *)

fun add (x, y) =
    let 
       val z = new()
       val mpfr_add = _import "mlton_mpfr_add" : t' * t' * t' -> int;
       val _ = withval3 mpfr_add (z, x, y)
    in
       z
    end

fun sub (x, y) =
    let 
       val z = new()
       val mpfr_sub = _import "mlton_mpfr_sub" : t' * t' * t' -> int;
       val _ = withval3 mpfr_sub (z, x, y)
    in
       z
    end

fun mul (x, y) =
    let 
       val z = new()
       val mpfr_mul = _import "mlton_mpfr_mul" : t' * t' * t' -> int;
       val _ = withval3 mpfr_mul (z, x, y)
    in
       z
    end

nonfix div

fun div (x, y) =
    let 
       val z = new()
       val mpfr_div = _import "mlton_mpfr_div" : t' * t' * t' -> int;
       val _ = withval3 mpfr_div (z, x, y)
    in
       z
    end

fun neg x =
    let
       val z = new()
       val mpfr_neg = _import "mlton_mpfr_neg" : t' * t' -> int;
       val _ = withval2 mpfr_neg (z, x)
    in 
       z
    end

fun inv x = div (one, x)

fun sqr x =
    let
       val z = new()
       val mpfr_sqr = _import "mlton_mpfr_sqr" : t' * t' -> int;
       val _ = withval2 mpfr_sqr (z, x)
    in 
       z
    end

fun sqrt x =
    let
       val z = new()
       val mpfr_sqrt = _import "mlton_mpfr_sqrt" : t' * t' -> int;
       val _ = withval2 mpfr_sqrt (z, x)
    in 
       z
    end

fun abs x =
    let
       val z = new()
       val mpfr_abs = _import "mlton_mpfr_abs" : t' * t' -> int;
       val _ = withval2 mpfr_abs (z, x)
    in 
       z
    end

fun atan x =
    let
       val z = new()
       val mpfr_atan = _import "mlton_mpfr_atan" : t' * t' -> int;
       val _ = withval2 mpfr_atan (z, x)
    in 
       z
    end

fun infinite x =
    let
       val mpfr_inf_p = _import "mlton_mpfr_inf_p" : t' -> int;
    in 
       withval1 mpfr_inf_p x <> 0
    end

fun finite x =
    let
       val mpfr_number_p = _import "mlton_mpfr_number_p" : t' -> int;
    in 
       withval1 mpfr_number_p x <> 0
    end

fun isZero x =
    let
       val mpfr_zero_p = _import "mlton_mpfr_zero_p" : t' -> int;
    in 
       withval1 mpfr_zero_p x <> 0
    end

fun isNan x =
    let
       val mpfr_nan_p = _import "mlton_mpfr_nan_p" : t' -> int;
    in 
       withval1 mpfr_nan_p x <> 0
    end

fun compare (x, y) = 
    let
       val mpfr_cmp = _import "mlton_mpfr_cmp" : t' * t' -> int;
       val n = withval2 mpfr_cmp (x, y)
    in
       if n > 0 then GREATER
       else if n = 0 then EQUAL
       else LESS
    end

val pi = mul(fromString "4.0", atan one)

val nan = div(zero, zero)

fun toReal x = 
    let
       val mpfr_get_d = _import "mlton_mpfr_get_d" : t' -> real;
    in
       withval1 mpfr_get_d x
    end

fun fromReal r =       
    let
       val x = new()
       val mpfr_set_d = _import "mlton_mpfr_set_d" : t' * real -> int;
       val _ = F.withValue(x, fn x => mpfr_set_d (x, r))
    in
       x
    end

end
