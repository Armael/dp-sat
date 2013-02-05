let bind opt f = match opt with
  | None -> None
  | Some x -> f x

let map f = function
  | None -> None
  | Some x -> Some (f x)

let promote x = function
  | None -> Some x
  | Some y -> Some y

let is_some = function
  | Some _ -> true
  | _ -> false

let iter f = function
  | Some i -> f i
  | None -> ()
