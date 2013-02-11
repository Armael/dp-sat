open Prelude
open Sigs

module Make = functor (C : Clauses) ->
struct
  module Clauses = C

  (* Set of clauses : will be our buckets *)
  module CSet = Set.Make
    (struct
      type t = C.t
      let compare = compare
     end)

  type bucket = CSet.t * CSet.t
  type t = bucket array

  let make n = Array.make n (CSet.empty, CSet.empty)

  (* Put a clause in the buckets.  There is a "i-1" because variables
     names start from 1 and not from 0 *)
  let put c a =
    if not (C.is_empty c) then 
      let i = abs (C.highest_var c) in
      let put_fun = if C.highest_var c > 0 then put_fst else put_snd in
      a.(i-1) <- put_fun a.(i-1) (CSet.add c)


  let get i a = a.(i-1)
  let set bucket i a = a.(i-1) <- bucket

  let iter f bucket =
    CSet.iter f (fst bucket);
    CSet.iter f (snd bucket)

  let iter_opposite f bucket =
    CSet.iter (fun c1 ->
      CSet.iter (fun c2 ->
        f c1 c2
      ) (snd bucket)
    ) (fst bucket)

  let filter f bucket = (CSet.filter f (fst bucket),
                         CSet.filter f (snd bucket))
  let is_empty bucket =
    (CSet.is_empty (fst bucket)) && (CSet.is_empty (snd bucket))
  let choose bucket =
    if not (CSet.is_empty (fst bucket)) then CSet.choose (fst bucket)
    else CSet.choose (snd bucket)
  let card bucket =
    (CSet.cardinal (fst bucket)) + (CSet.cardinal (snd bucket))
end
