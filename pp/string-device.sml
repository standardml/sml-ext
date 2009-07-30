(* 
 * An inefficient pretty-printing string device 
 *)

structure StringDevice :> sig
  include PP_DEVICE
  val new_device : {wid : int} -> device
  val to_string : device -> string
end = 
struct

  type device = {wid : int,
                 s : string ref}

  fun new_device {wid} : device = {wid = wid,
                                   s = ref ""}

  fun to_string ({s,...}:device) = !s

  (* no style support *)
  type style = unit
  fun sameStyle _ = true
  fun pushStyle _ = ()
  fun popStyle _ = ()
  fun defaultStyle _ = ()

  (* maximum printing depth (in terms of boxes) *)
  fun depth _ = NONE

  (* the width of the device *)
  fun lineWidth ({wid, ...}:device) = SOME wid

  (* the suggested maximum width of text on a line *)
  fun textWidth _ = NONE

  (* output some number of spaces to the device *)
  fun space ({s,...}:device, n) = s := !s ^ StringCvt.padLeft #" " n ""

  (* output a new-line to the device *)
  fun newline ({s,...}:device) =  s := !s ^ "\n"

  (* output a string/character in the current style to the device *)
  fun string ({s,...}:device, s') = s := !s ^ s'

  fun char ({s,...}:device, c) = s := !s ^ str c

  (* if the device is buffered, then flush any buffered output *)
  fun flush _ = ()

end

