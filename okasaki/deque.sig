signature DEQUE =
sig
  type 'a Queue

  val empty   : 'a Queue
  val isEmpty : 'a Queue -> bool

  (* insert, inspect, and remove the front element *)
  val cons    : 'a * 'a Queue -> 'a Queue
  val head    : 'a Queue -> 'a         (* raises Empty if queue is empty *)
  val tail    : 'a Queue -> 'a Queue   (* raises Empty if queue is empty *)

  (* insert, inspect, and remove the rear element *)
  val snoc    : 'a Queue * 'a -> 'a Queue
  val last    : 'a Queue -> 'a         (* raises Empty if queue is empty *)
  val init    : 'a Queue -> 'a Queue   (* raises Empty if queue is empty *)
end

signature CATENABLE_DEQUE =
sig
  type 'a Cat

  val empty   : 'a Cat
  val isEmpty : 'a Cat -> bool

  val cons    : 'a * 'a Cat -> 'a Cat
  val head    : 'a Cat -> 'a       (* raises Empty if deque is empty *)
  val tail    : 'a Cat -> 'a Cat   (* raises Empty if deque is empty *)

  val snoc    : 'a Cat * 'a -> 'a Cat
  val last    : 'a Cat -> 'a       (* raises Empty if deque is empty *)
  val init    : 'a Cat -> 'a Cat   (* raises Empty if deque is empty *)

  val ++      : 'a Cat * 'a Cat -> 'a Cat
end
