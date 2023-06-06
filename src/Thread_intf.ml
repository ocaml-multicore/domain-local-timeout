(** Signature for a minimal subset of the [Stdlib.Thread] module needed by
    domain local timeout. *)
module type Thread = sig
  type t

  val self : unit -> t
  val id : t -> int
  val create : ('a -> 'b) -> 'a -> t
  val join : t -> unit
end

type thread = (module Thread)
(** Type alias for a first-class {!Thread} module. *)
