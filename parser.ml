open Prelude

let parse_clause s =
  let i = ref 0 in
  let vars = ref [] in
  try
    while true do
      Scanf.bscanf 
        (Scanf.Scanning.from_function
           (fun () ->
             if !i < String.length s then (
               let c = s.[!i] in
               incr i; c
             ) else raise End_of_file
           ))
        " %d"
        (fun x -> vars := x::!vars)
    done; assert false (* never reached *)
  with End_of_file -> List.tl !vars (* remove the final '0' *)

let parse filename =
  let chan = open_in filename in

  let clauses = ref [] in
  let nb_vars = ref 0
  and nb_clauses = ref 0 in
  
  (try
     while true do
       let line = input_line chan in
       let i = strip_begin line in
       Option.iter (fun i -> match line.[i] with
       | 'p' -> let (v, c) = Scanf.sscanf line "p cnf %d %d" (fun x y -> (x,y)) in
                nb_vars := v;
                nb_clauses := c
       | 'c' -> ()
       | _ -> (* So it's a clause *)
         clauses := (parse_clause line)::!clauses) i
     done
   with End_of_file -> ());
  close_in chan;
  (!nb_vars, !nb_clauses, !clauses)
