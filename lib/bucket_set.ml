open Sigs

module Make = functor (C : Clauses) ->
struct
  module Clauses = C

  module CSet = Set.Make
    (struct
      type t = C.t
      let compare = C.compare
     end)

  include CSet
end
