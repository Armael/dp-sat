open Prelude

module Sat = Putnam.Solver
  (struct
    let compare = compare
    let verbosity = 1
   end)

let _ =
  if Array.length Sys.argv = 1 then
    die "You must provide a file in cnf syntax"
  else
    let (nb_vars, nb_clauses, clauses) = Parser.parse Sys.argv.(1) in

    (match Sat.sat nb_vars nb_clauses clauses with
    | Sat.Unsat -> Printf.printf "Unsat"
    | Sat.Sat l -> Printf.printf "Sat\n";
      Pretty.list (Pretty.list Pretty.int) l);
    print_newline ()
