open Prelude
open Sigs

module Make = functor (O : OrderInt) ->
struct
  (* O gives us an order over the integers [1..nb_vars] (it's value for
     other values doesn't matters). We want to refine it to order the
     variables by their number (their "absolute value"), then by their
     "polarity" (their sign).

     To do that we construct a lexigographic product of these two
     orders *)

  (* Takes a list of orders (comparison functions, like compare)
     sorted by priority and returns the lexicographic order for this
     orders *)
  let lexico orders = (fun x y ->
    let rec ord = function
      | [] -> assert false
      | [o] -> o x y
      | o::os -> let c = o x y in
                 if c = 0 then ord os
                 else c in
    ord orders)

  (* We use the order O on the integers to order variables by they
     index. We also define an order between x and (not x) : x is
     first. *)
  let compare_vars =
    (* We first sort by index of the variable, and in case of equality, x
       comes before (not x) *)
    lexico [(fun x y -> O.compare (abs y) (abs x));
            (fun x y -> compare y x)] (* put x before -x *)

  (* We export our newly created comparison function over the
     variables *)
  module Order = struct
    let compare = compare_vars
  end

  exception Empty
  let detect_empty_clauses = ref false

  module LitSet = Set.Make
    (struct
      type t = int
      let compare = (fun x y ->
        if !detect_empty_clauses && x = -y then
          raise Empty
        else compare_vars x y)
     end)

  type t = LitSet.t

  (* Because our clauses are implemented as AVLs, the structural
     standard comparison doesn't works here : there are more than one
     balanced trees that can represent the same set. *)
  let compare = LitSet.compare

  let import list =
    detect_empty_clauses := true;
    let c = 
      try (
        Some (List.fold_left (fun set v -> LitSet.add v set) LitSet.empty list)
      ) with Empty -> None in
    detect_empty_clauses := false;
    c

  let export = LitSet.elements
  let highest_var = LitSet.min_elt
  let is_empty = LitSet.is_empty

  let merge c1 c2 =
    try (
      let x1 = LitSet.min_elt c1 and x2 = LitSet.min_elt c2 in
      detect_empty_clauses := true;
      let u = LitSet.union (LitSet.remove x1 c1) (LitSet.remove x2 c2) in
      detect_empty_clauses := false;
      if u = LitSet.empty then None
      else Some u
    ) with Empty -> Some (LitSet.empty)

  let iter = LitSet.iter

  let subset = LitSet.subset
end
  
