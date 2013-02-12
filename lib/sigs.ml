module type OrderInt = sig
  val compare : int -> int -> int
end

module type Clauses = sig
  module Order : OrderInt

  type t
  val compare : t -> t -> int
  val import : int list -> t option
  val export : t -> int list
  val highest_var : t -> int
  val is_empty : t -> bool
  val merge : t -> t -> t option
  val iter : (int -> unit) -> t -> unit
  val subset : t -> t -> bool
end

module type Bucket =
sig
  module Clauses : Clauses

  type t
  val empty : t
  val add : Clauses.t -> t -> t
  val iter : (Clauses.t -> unit) -> t -> unit
  val fold : (Clauses.t -> 'a -> 'a) -> t -> 'a -> 'a
  val filter : (Clauses.t -> bool) -> t -> t
  val is_empty : t -> bool
  val choose : t -> Clauses.t
  val cardinal : t -> int
end

module type Buckets =
sig
  module Clauses : Clauses

  type t
  val make : int -> t
  val put : Clauses.t -> t -> unit

  type bucket
  val get : int -> t -> bucket
  val set : bucket -> int -> t -> unit
  val iter : (Clauses.t -> unit) -> bucket -> unit
  val iter_opposite : (Clauses.t -> Clauses.t -> unit) -> bucket -> unit    
  val filter : (Clauses.t -> bool) -> bucket -> bucket
  val is_empty : bucket -> bool
  val choose : bucket -> Clauses.t
  val card : bucket -> int
end

module type Solver =
sig
  type output =
  | Unsat
  | Sat of int list

  val sat : int -> int -> int list list -> output
end
