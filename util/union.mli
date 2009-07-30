(*
 *      $Id: union.mli,v 1.3 2003/11/12 17:26:01 bjeannet Exp $
 *
 *      Union-find Abstract Data Types
 *)

(** Union-find Abstract Data Types *)

type 'a t
        (** The type of the data structure storing set membership (the universe) *)

val create: int -> 'a t
        (** Create a new universe set *)
val add:  'a t -> 'a -> unit
        (** Add an element to the universe (initially belonging to its singleton) *)
val find: 'a t -> 'a -> 'a
        (* Find the set of an element *)
val union: 'a t -> 'a -> 'a -> 'a
        (** Computes the union of two sets and returns the resulting set *)
val extract: 'a t -> 'a list list
        (** Extract the list of sets *)

