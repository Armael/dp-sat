open Prelude

module Sat = Putnam

let _ =
  if Array.length Sys.argv = 1 then
    die "You must provide a file in cnf syntax"
  else
    let (nb_vars, nb_clauses, clauses) = Parser.parse Sys.argv.(1) in

    (match Sat.sat nb_vars nb_clauses clauses with
    | Sat.Unsat -> Printf.printf "Unsat"
    | Sat.Sat l -> Printf.printf "Sat\n";
      Pretty.list (Pretty.list Pretty.int) l
    | Sat.Unknown -> Printf.printf "Unknown");
    print_newline ()
