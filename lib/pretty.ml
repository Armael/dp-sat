(* Some pretty-printing utility functions.

   A cleaner solution would be to use sexplib from Janestreet's Core
   library, but I don't want here to add extra dependancies to this
   little project
*)

let int = print_int;;

let list pp_item l =
  let rec print_content = function
  | [] -> ()
  | [x] -> pp_item x
  | x::xs -> pp_item x;
    Printf.printf "; ";
    print_content xs in

  Printf.printf "[";
    print_content l;
    Printf.printf "]"

let int_list = list int
