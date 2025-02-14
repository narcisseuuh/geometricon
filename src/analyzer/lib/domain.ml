include Frontend.Ast

module type Domain_t =
sig
  type t

  val init : init -> t
  val to_t : abstract_set * abstract_set -> t

  val bottom : unit -> t

  val join : t -> t -> t
  val widen : t -> t -> t

  val translate : u:float -> v:float -> t -> t
  val rotate : u:float -> v:float -> theta:float -> t -> t

  val subset : t -> t -> bool
  val is_bottom : t -> bool
  val meet : t -> t -> t

  val print : Format.formatter -> t -> unit
end