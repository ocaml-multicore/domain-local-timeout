(** Signature for a minimal subset of the [Stdlib.Unix] module needed by domain
    local timeout. *)
module type Unix = sig
  val gettimeofday : unit -> float

  type file_descr

  val read : file_descr -> bytes -> int -> int -> int
  val write : file_descr -> bytes -> int -> int -> int
  val pipe : ?cloexec:bool -> unit -> file_descr * file_descr

  val select :
    file_descr list ->
    file_descr list ->
    file_descr list ->
    float ->
    file_descr list * file_descr list * file_descr list
end

type unix = (module Unix)
(** Type alias for a first-class {!Unix} module. *)
