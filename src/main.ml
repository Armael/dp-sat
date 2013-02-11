open Prelude
open Sigs

(* We choose a standard order on the variables' index *)
module O = struct
  let compare = compare
end

module Sat = Dp.Solver
  (struct let verbosity = 1 end)
  (Buckets.Make (Clauses.Make (O)))

let _ =
  if Array.length Sys.argv = 1 then
    die "You must provide a file in cnf syntax"
  else
    let (nb_vars, nb_clauses, clauses) = Parser.parse Sys.argv.(1) in

    (match Sat.sat nb_vars nb_clauses clauses with
    | Sat.Unsat -> Printf.printf "s UNSATISFIABLE"
    | Sat.Sat l -> Printf.printf "s SATISFIABLE\n";
      if Verif.verif nb_vars nb_clauses clauses l then
        Printf.printf "c Verification OK\n"
      else Printf.printf "c Verification FAILED\n";
      List.iter (fun v -> Printf.printf "v %d\n" v) l);

