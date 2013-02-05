open Prelude

let _ = 
  if Array.length Sys.argv < 4 then
    die "Usage : generator <nb vars> <nb clauses> <clauses size>
Outputs a instance in cnf syntax with <nb vars> variables
and <nb clauses> clauses of <clauses size> size";

  let nb_vars = int_of_string Sys.argv.(1) in
  let nb_clauses = int_of_string Sys.argv.(2) in
  let clauses_size = int_of_string Sys.argv.(3) in

  Generate.write_instance nb_vars nb_clauses clauses_size stdout
