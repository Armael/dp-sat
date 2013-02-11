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
    | Sat.Unsat -> Printf.printf "Unsat"
    | Sat.Sat l -> Printf.printf "Sat\n";
      Pretty.int_list l);
    print_newline ()
