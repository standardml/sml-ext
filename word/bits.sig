
(* All conversions are big-endian. *)
signature BITS =
sig

structure Ops: sig 
   datatype bit = O | I
end

val bitToString: Ops.bit -> string

val bitsToWord8vector: Ops.bit list -> Word8Vector.vector
val word8vectorToBits: Word8Vector.vector -> Ops.bit list 
val word8vectorToString: Word8Vector.vector -> string
val intToBits: int -> Ops.bit list

end
