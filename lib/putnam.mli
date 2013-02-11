module type Params = sig
  val compare : int -> int -> int
  val verbosity : int
end

module Solver : functor (P : Params) ->
sig
  type output =
  | Unsat
  | Sat of int list

  val sat : int -> int -> int list list -> output
end
