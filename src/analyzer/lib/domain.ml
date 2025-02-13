include Frontend.Ast

module type Domain_t =
sig
  type t

  val init : init -> t

  val bottom : unit -> t

  val join : t -> t -> t
  val widen : t -> t -> t

  val subset : t -> t -> bool
  val is_bottom : t -> bool

  val print : Format.formatter -> t -> unit
end