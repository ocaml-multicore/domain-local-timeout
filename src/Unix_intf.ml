(** Signature for a minimal subset of the [Stdlib.Unix] module needed by domain
    local timeout. *)
module type Unix = sig
  type file_descr

  val close : file_descr -> unit
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
