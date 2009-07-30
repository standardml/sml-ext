
structure MPFI :> MPFI =
struct 

structure A = Array
structure S = StringExt
structure F = MLton.Finalizable
structure P = MLton.Pointer
structure M = MPFR

open GeneralExt

type ptr = P.t 
type t' = ptr
type t = t' F.t

exception MPFI

(* -------------------------------------------------------------------------- *)
(*  Util                                                                      *)
(* -------------------------------------------------------------------------- *)

fun withval1 f x = F.withValue(x,f)
fun withval2 f (x1, x2) = F.withValue(x1, fn x1 => F.withValue (x2, fn x2 => f(x1, x2)))
fun withval3 f (x1, x2, x3) = F.withValue(x1, fn x1 => 
                              F.withValue(x2, fn x2 => 
                              F.withValue(x3, fn x3 => f(x1, x2, x3))))

(* -------------------------------------------------------------------------- *)
(*  Interface                                                                 *)
(* -------------------------------------------------------------------------- *)

fun new () = 
    let
       val new = _import "mlton_mpfi_new" : unit -> t';
       val free = _import "mlton_mpfi_free" : t' -> unit;
       val i = F.new (new())
    in
       F.addFinalizer(i, free);
       i
    end

fun toString x = 
    let
       val to_string = _import "mlton_mpfi_to_string" : t' -> char array;
       val s = withval1 to_string x
       val s = A.vector s
       val s = S.truncateCString s
    in
       s
    end

fun fromString s =
    let
       val z = new()
       val mpfi_set_str = _import "mlton_mpfi_set_str" : t' * string -> int;
       val s = S.nullTerminate (S.concat["[", s, ",", s, "]"])
       val n = F.withValue(z, fn z => mpfi_set_str(z, s))
    in
       if n = 0 then z else raise MPFI
    end       

val fromInt = fromString o Int.toString 

fun add (x, y) =
    let 
       val z = new()
       val mpfi_add = _import "mlton_mpfi_add" : t' * t' * t' -> int;
       val _ = withval3 mpfi_add (z, x, y)
    in
       z
    end

fun sub (x, y) =
    let 
       val z = new()
       val mpfi_sub = _import "mlton_mpfi_sub" : t' * t' * t' -> int;
       val _ = withval3 mpfi_sub (z, x, y)
    in
       z
    end

fun mul (x, y) =
    let 
       val z = new()
       val mpfi_mul = _import "mlton_mpfi_mul" : t' * t' * t' -> int;
       val _ = withval3 mpfi_mul (z, x, y)
    in
       z
    end

nonfix div 

fun div (x, y) =
    let 
       val z = new()
       val mpfi_div = _import "mlton_mpfi_div" : t' * t' * t' -> int;
       val _ = withval3 mpfi_div (z, x, y)
    in
       z
    end

fun neg x =
    let
       val z = new()
       val mpfi_neg = _import "mlton_mpfi_neg" : t' * t' -> int;
       val _ = withval2 mpfi_neg (z, x)
    in 
       z
    end

fun sqr x =
    let
       val z = new()
       val mpfi_sqr = _import "mlton_mpfi_sqr" : t' * t' -> int;
       val _ = withval2 mpfi_sqr (z, x)
    in 
       z
    end

fun sqrt x =
    let
       val z = new()
       val mpfi_sqrt = _import "mlton_mpfi_sqrt" : t' * t' -> int;
       val _ = withval2 mpfi_sqrt (z, x)
    in 
       z
    end

fun inv x =
    let
       val z = new()
       val mpfi_inv = _import "mlton_mpfi_inv" : t' * t' -> int;
       val _ = withval2 mpfi_inv (z, x)
    in 
       z
    end

fun abs x =
    let
       val z = new()
       val mpfi_abs = _import "mlton_mpfi_abs" : t' * t' -> int;
       val _ = withval2 mpfi_abs (z, x)
    in 
       z
    end

fun atan x =
    let
       val z = new()
       val mpfi_atan = _import "mlton_mpfi_atan" : t' * t' -> int;
       val _ = withval2 mpfi_atan (z, x)
    in 
       z
    end

fun finite x =
    let
       val mpfi_bounded_p = _import "mlton_mpfi_bounded_p" : t' -> int;
    in 
       withval1 mpfi_bounded_p x <> 0
    end

fun isZero x =
    let
       val mpfi_is_zero = _import "mlton_mpfi_is_zero" : t' -> int;
    in 
       withval1 mpfi_is_zero x > 0
    end

val zero = fromString "0.0"
val one = fromString "1.0"
val pi = mul(fromString "4.0", atan one)
val nan = div(zero, zero)

fun compare (x, y) = 
    let
       val mpfi_cmp = _import "mlton_mpfi_cmp" : t' * t' -> int;
       val n = withval2 mpfi_cmp (x, y)
    in 
       if n < 0 then SOME LESS 
       else if n > 0 then SOME GREATER
       else NONE 
    end

fun left x =
    let
       val mpfi_get_left = _import "mlton_mpfi_get_left" : t' -> ptr;
    in 
       M.fromPtr (withval1 mpfi_get_left x)
    end

fun right x =
    let
       val mpfi_get_right = _import "mlton_mpfi_get_right" : t' -> ptr;
    in 
       M.fromPtr (withval1 mpfi_get_right x)
    end

fun make (l, r) = 
    let
       val x = new()
       val mpfi_interv_fr = _import "mlton_mpfi_interv_fr" : t' * ptr * ptr -> int;
       val _ = F.withValue(x, fn x => M.asPtr(l, 
                              fn l => M.asPtr(r, 
                              fn r => mpfi_interv_fr (x, l, r))))

    in 
       x
    end

end
