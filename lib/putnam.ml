open Prelude

(****************
 * Parameters
 ****************)

(* You can choose any custom order on the variables, defined by a
   comparison function (like Pervasives.compare, used here) *)
let compare = compare

(* For debug output.
   0 means "No debug output",
   1 prints only the number of clauses in the buckets,
   2 prints also the merge of all clauses
*)
let verbosity = 1

(****************)


(* Prints the debug message by calling the display function (unit ->
   unit) if verbosity >= level *)
let debug level display =
  if verbosity >= level then
    display ()

(* Takes a list of orders (comparison functions, like compare) sorted
   by priority and returns the lexicographic order for this orders *)
let lexico orders = (fun x y ->
  let rec ord = function
    | [] -> assert false
    | [o] -> o x y
    | o::os -> let c = o x y in
               if c = 0 then ord os
               else c in
  ord orders)

(* We use the standard order on the integers to order variables by
   they index. We also define an order between x and (not x) : x is
   first. *)
let compare_vars =
  (* We first sort by index of the variable, and in case of equality, x
     comes before (not x) *)
  lexico [(fun x y -> compare (abs y) (abs x)); 
          (fun x y -> Pervasives.compare y x)] (* put x before -x *)

let remove_dup_opp l =
  let rec aux acc = function
    | [] -> List.rev acc
    | [x] -> List.rev (x::acc)
    | x::y::xs -> if x = y then aux (x::acc) xs
      else if x = -y then []
      else aux (x::acc) (y::xs) in
  let l' = aux [] l in
  l'

(* Some pretreatmets on the clauses list to optimize things *)
let pre clauses =
  (* Knowing the number of the maximal variable will be useful for the
     algorithm, so we sort the variables of the clauses in inverse
     order.

     We also "clean" the clauses : if a variable is present two times
     (who knows ?) we remove the double occurence, and if a clause
     contains (x AND (NOT x)) we remove it (it is automatically
     satisfiable).
  *)
  let sort c = List.sort compare_vars c in
  (* map_skip removes empty clauses *)
  map_skip (fun c -> c |> sort |> remove_dup_opp) clauses

(* Raised when we find an empty clause in the buckets *)
exception Unsat_exc

type output =
| Unsat
| Sat of int list list
| Unknown

let sat nb_vars nb_clauses clauses =
  let clauses = pre clauses in

  (* Here we implement a small optimization : we have, in place of a
     single bucket, a double bucket. For a given variable x, the first
     bucket contains clauses containing x, the second the clauses
     containing -x. This way, we don't have to try to merge a
     x-containing-clause with an other x-containing-clause (same with
     -x) *)
  let buckets = Array.make nb_vars ([], []) in
  
  (* Put clauses in the buckets.
     There is a "i-1" because variables names start from 1 and not
     from 0 *)
  let put clause =
    let i = abs (List.hd clause) in
    let concat = (fun cs -> clause::cs) in
    if List.hd clause > 0 then
      buckets.(i-1) <- put_fst buckets.(i-1) concat
    else 
      buckets.(i-1) <- put_snd buckets.(i-1) concat in

  let clean i =
    let cln b = remove_dup (List.sort Pervasives.compare b) in
    buckets.(i) <- map2 cln buckets.(i) in

  let nb_clauses b =
    (List.length (fst b)) + (List.length (snd b)) in

  List.iter put clauses;

  (* We have to know in each order examine the variables, so we sort
     [0;…;nb_vars-1] (the index of the buckets) according to
     compare_vars
  *)
  let buckets_seq = List.sort compare_vars (seq 0 (nb_vars - 1)) in

  try (
    List.iter (fun k ->

      debug 1 (fun () -> Printf.printf
        "Current buckets : %d. %d clauses before cleaning" k (nb_clauses buckets.(k)));

      (* We remove duplicated clauses in the bucket before we iterate on it *)
      clean k;

      debug 1 (fun () -> Printf.printf
        ", %d after\n%!" (nb_clauses buckets.(k)));
      debug 2 (fun () -> Printf.printf "Contenu : \n";
        List.iter (fun c -> Pretty.list Pretty.int c; print_newline ()) (fst buckets.(k));
        List.iter (fun c -> Pretty.list Pretty.int c; print_newline ()) (snd buckets.(k)));
      
      List.iter (fun c1 ->
        List.iter (fun c2 ->
          (* Because of the "double-bucket mechanism", we know that c1
             is of the form [k, ...] and c2 : [-k, ...] so we can
             merge them *)
          debug 3 (fun () -> Pretty.list Pretty.int c1; print_string " -- ";
            Pretty.list Pretty.int c2);
          (try (
            let merged = merge_sorted_tr compare_vars (List.tl c1) (List.tl c2) in
            if merged = [] then raise Unsat_exc
            else (
              let merged = remove_dup_opp merged in
              if merged <> [] then (
                debug 3 (fun () -> 
                  print_string " -> "; Pretty.list Pretty.int merged);
                put merged;
              )
            )
           ) with Failure "hd" -> raise Unsat_exc); (* We found an empty clause
                                                       somewhere *)
          debug 3 (fun () -> print_endline "");
        ) (snd buckets.(k))
      ) (fst buckets.(k));

    ) buckets_seq;
    (* Here we know the problem is SAT. We have to find an assignation
       of the variables *)
    Sat []
  ) with Unsat_exc -> Unsat
