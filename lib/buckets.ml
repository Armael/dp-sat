open Prelude
open Sigs

module Make = functor (B : Bucket) ->
struct
  module Clauses = B.Clauses

  type bucket = B.t * B.t
  type t = bucket array

  let make n = Array.make n (B.empty, B.empty)

  (* Put a clause in the buckets.  There is a "i-1" because variables
     names start from 1 and not from 0 *)
  let put c a =
    if not (Clauses.is_empty c) then 
      let i = abs (Clauses.highest_var c) in
      let put_fun = if Clauses.highest_var c > 0 then put_fst else put_snd in
      a.(i-1) <- put_fun a.(i-1) (B.add c)


  let get i a = a.(i-1)
  let set bucket i a = a.(i-1) <- bucket

  let iter f bucket =
    B.iter f (fst bucket);
    B.iter f (snd bucket)

  let iter_opposite f bucket =
    B.iter (fun c1 ->
      B.iter (fun c2 ->
        f c1 c2
      ) (snd bucket)
    ) (fst bucket)

  let filter f bucket = (B.filter f (fst bucket),
                         B.filter f (snd bucket))
  let is_empty bucket =
    (B.is_empty (fst bucket)) && (B.is_empty (snd bucket))
  let choose bucket =
    if not (B.is_empty (fst bucket)) then B.choose (fst bucket)
    else B.choose (snd bucket)
  let card bucket =
    (B.cardinal (fst bucket)) + (B.cardinal (snd bucket))
end
