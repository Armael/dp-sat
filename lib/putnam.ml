open Prelude
open Sigs

(* Parameters module *)
module type Params = sig
  (* Verbosity for debug messages.
     For debug output.
     0 means no debug output,
     1 prints only the number of clauses in the buckets,
     2 also prints the content of the buckets
     3 also prints the merge of all clauses
  *)
  val verbosity : int
end

module Solver =
  functor (P : Params) ->
    functor (Buckets : Buckets) ->
struct
  (* Prints the debug message by calling the display function (unit ->
     unit) if verbosity >= level *)
  let debug level display =
    if P.verbosity >= level then
      display ()

  (* Raised when we find an empty clause in the buckets *)
  exception Unsat_exc

  type output =
  | Unsat
  | Sat of int list

  let sat nb_vars nb_clauses clauses =
    (* Convert int-list clauses to B.Clauses.t clauses *)
    let clauses = map_some Buckets.Clauses.import clauses in

    (* Here we implement a small optimization : we have, instead of a
       single bucket, a double bucket. For a given variable x, the first
       bucket contains clauses with head x, the second the clauses with
       head -x. This way, we don't have to try to merge a
       x-containing-clause with an other x-containing-clause (same with
       -x) *)
    let buckets = Buckets.make nb_vars in
    
    List.iter (fun c -> Buckets.put c buckets) clauses;

    (* We have to know in each order examine the variables, so we sort
       [1;…;nb_vars] (the index of the variables) according to
       the order on the variables *)
    let buckets_seq = List.sort Buckets.Clauses.Order.compare (seq 1 nb_vars) in

    try (
      List.iter (fun k ->
        let bucket = Buckets.get k buckets in

        debug 1 (fun () -> Printf.printf
          "Current bucket : %d (%d clauses)\n%!" k
          (Buckets.card bucket));

        debug 2 (fun () -> Printf.printf "Contenu : \n";
          Buckets.iter (fun c -> Pretty.int_list (Buckets.Clauses.export c);
            print_newline ())
            bucket);

        Buckets.iter_opposite (fun c1 c2 ->
          (* Because of the "double-bucket mechanism", we know that c1
             is of the form [k, ...] and c2 : [-k, ...] so we can
             merge them *)
          debug 3 (fun () -> Pretty.int_list (Buckets.Clauses.export c1);
            print_string " -- ";
            Pretty.int_list (Buckets.Clauses.export c2));

          (match Buckets.Clauses.merge c1 c2 with
          | Some c -> debug 3 (fun () -> print_string " -> ";
            Pretty.int_list (Buckets.Clauses.export c));
            Buckets.put c buckets
          | None -> raise Unsat_exc);

          debug 3 (fun () -> print_endline "")
        ) bucket
      ) buckets_seq;

      (* Here we know the problem is SAT. We have to find an assignment
         of the variables *)
      let assign = Hashtbl.create nb_vars in
      let buckets_seq = List.rev buckets_seq in
      (* Hashtbl.find modified to fix an arbitrary value (and return
         it) in case of fail *)
      let find x = try Hashtbl.find assign x with
          Not_found -> Hashtbl.add assign x true; true in

      List.iter (fun k ->
        let bucket = Buckets.get k buckets in

        (* Set of clauses unsatisfied with only the assignment of
           precedent variables *)
        let unsat =
          Buckets.filter (fun c -> 
            let sat = ref false in
            Buckets.Clauses.iter (fun var ->
              if abs var <> Buckets.Clauses.highest_var c then
                sat := !sat || (if var > 0 then find var
                  else not (find (-var)))
            ) c;
            not !sat
          ) bucket in
        if Buckets.is_empty unsat then
          (* We can use any value of our choice for variable k
             (variables start from 1) as all the clauses are
             satisfied *)
          Hashtbl.add assign k true
        else
          let x = Buckets.Clauses.highest_var (Buckets.choose unsat) in
          Hashtbl.add assign (abs x) (x > 0)
      ) buckets_seq;

      Sat (List.map (fun v ->
        if Hashtbl.find assign v then v else -v)
             buckets_seq)
    ) with Unsat_exc -> Unsat
end
