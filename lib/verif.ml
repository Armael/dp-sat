open Prelude

(* Returns true if the list of values 'values' satisfy the clause, and
   false otherwise.

   The list 'values' contains integers : -3 means for example that the
   variable 3 is false
*)
let verif nb_vars nb_clauses clauses values =
  let tbl = Hashtbl.create nb_vars in

  (* Initialize a hashtable with the values of the variables *)
  List.iter (fun v ->
    if v > 0 then Hashtbl.add tbl v true
    else Hashtbl.add tbl (abs v) false) values;

  let verif_clause clause =
    List.fold_left (fun acc v ->
      (if v > 0 then (Hashtbl.find tbl v)
       else not (Hashtbl.find tbl (abs v))) || acc) false clause in

  fold_stop (fun acc c ->
    let nacc = acc && (verif_clause c) in
    (nacc, nacc)) true clauses
