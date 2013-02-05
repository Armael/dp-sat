let write_instance nb_vars nb_clauses clauses_size chan =
  Printf.fprintf chan "p cnf %d %d\n" nb_vars nb_clauses;
  
  Random.self_init ();
  for i = 0 to nb_clauses - 1 do
    for j = 0 to clauses_size - 1 do
      let var = (if Random.bool () then 1 else -1) * (Random.int nb_vars + 1) in
      Printf.fprintf chan "%d " var
    done;
    Printf.fprintf chan "0\n"
  done


