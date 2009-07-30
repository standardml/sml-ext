(*
 *      $Id: union.ml,v 1.2 2003/11/12 17:26:01 bjeannet Exp $
 *
 *      Union-find Abstract Data Types
 *)

(** Union-find Abstract Data Types *)

type 'a t = ('a,'a) Hashtbl.t
        (** The type of the sets of all elements *)

let create = Hashtbl.create

let add tbl elt = Hashtbl.add tbl elt elt

let find tbl element =
  let parent = Hashtbl.find tbl element in
  if parent=element then
    element
  else begin
    let parent2 = Hashtbl.find tbl parent in
    if parent2 = parent then
      parent
    else begin
      (* The find function *)
      let rec findrec element =
	let parent = Hashtbl.find tbl element in
	if parent=element then
	  element
	else
	  findrec parent
      in
      (* The path compression function *)
      let rec compressrec res element =
	let parent = Hashtbl.find tbl element in
	if parent = element then 
	  ()
	else begin
	  Hashtbl.replace tbl element res;
	  compressrec res parent
	end
      in
      (* The algorithm *)
      let res = findrec parent2 in
      compressrec res element;
      res
    end
  end

let union tbl element1 element2 =
  let root1 = find tbl element1 and
      root2 = find tbl element2
  in 
  Hashtbl.replace tbl root1 root2;
  root2

let extract tbl =
  let size = 
    let size = ref 0 in
    Hashtbl.iter (fun elt _ -> incr size) tbl;
    !size
  in
  let tbl2 = 
    let tbl2 = Hashtbl.create size in
    Hashtbl.iter (fun elt _ -> Hashtbl.add tbl2 elt []) tbl;
    tbl2
  in
  Hashtbl.iter
    (begin fun elt parent ->
      let parent = find tbl parent in
      let list = Hashtbl.find tbl2 parent in
      Hashtbl.replace tbl2 parent (elt::list)
    end)
    tbl;
  let res = ref [] in
  Hashtbl.iter
    (fun _ list -> if list<>[] then res := list :: !res)
    tbl2;
  !res
