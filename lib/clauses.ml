open Prelude
open Sigs

module Make = functor (O : OrderInt) ->
struct
  type t = int list
    
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

  (* Order on the clauses : we can use here the standard structural
     order *)
  let compare = compare

  (* Removes the duplicated variables in a clause. Moreover, if the
     clause contains x and -x, [] is returned *)
  let remove_dup_opp l =
    let rec aux acc = function
      | [] -> List.rev acc
      | [x] -> List.rev (x::acc)
      | x::y::xs -> if x = y then aux (x::acc) xs
        else if x = -y then []
        else aux (x::acc) (y::xs) in
    let l' = aux [] l in
    l'

  let import list =
    let sort = List.sort compare_vars in
    let opt_of_list = function [] -> None | l -> Some l in
    list |> sort |> remove_dup |> remove_dup_opp |> opt_of_list

  let export c = c
        
  let highest_var c = List.hd c
  let is_empty c = (c = [])

  let merge c1 c2 =
    try (
      let merged = merge_sorted compare_vars (List.tl c1) (List.tl c2) in
      if merged = [] then failwith ""
      else Some (remove_dup_opp merged)
    ) with Failure _ -> None

  let iter = List.iter

  (* Use clauses_set to have subset optimization *)
  let subset c1 c2 = failwith "Not implemented : you may want to use Clauses_set"
end
