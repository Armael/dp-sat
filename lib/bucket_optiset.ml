open Sigs

module Make = functor (C : Clauses) ->
struct
  module B = (Bucket_set.Make (C))
  include B

  let add c b =
    let added = ref false in
    let b = fold (fun clause acc ->
      if C.subset clause c then (
        if not !added then (
          added := true;
        );
        B.add clause acc
      ) else if C.subset c clause then (
        if not !added then (
          added := true;
          B.add c acc
        ) else (
          acc
        )
      ) else B.add clause acc
    ) b empty in
    let b = if not !added then B.add c b else b in
    b
end
