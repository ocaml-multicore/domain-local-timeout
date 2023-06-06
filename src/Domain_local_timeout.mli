(** {1 Interface for timeouts} *)

type cancel = private unit -> unit
(** Private type alias for [unit -> unit] to prevent spurious warnings. *)

val set_timeoutf : float -> (unit -> unit) -> cancel
(** [let cancel = set_timeoutf seconds action] registers the [action] to be
    called after the specified time period in [seconds] has passed.  The return
    value [cancel] is an idempotent and domain safe action that can be called
    as [(cancel :> unit -> unit) ()] to cancel the timeout.

    {b NOTE}: The [action] must not raise exceptions or perform effects, should
    not block, and should usually just perform some minimal side-effect to
    e.g. unblock a fiber to do the work.

    {b WARNING}: It is allowed for the given [action] to be called e.g. from
    another thread outside of the scheduler from which [set_timeoutf] was
    called. *)

(** {1 Interface for schedulers} *)

val using :
  set_timeoutf:(float -> (unit -> unit) -> unit -> unit) ->
  while_running:(unit -> 'a) ->
  'a
(** [using ~set_timeout ~while_running] registers the given timeout mechanism
    for the current domain for the duration of running the given scheduler. *)

(** {1 Default timeout implementation} *)

include module type of Thread_intf
include module type of Unix_intf

val set_system : thread -> unix -> unit
(** [set_system (module Thread) (module Unix)] sets the system libraries to use
    for a default {!set_timeoutf} implementation.  This operation has an effect
    only on first time and, if needed, should be called from application code on
    the main domain before code using timeouts is called.

    {b NOTE}: The default {!set_timeoutf} may not always be needed.  It is
    possible for schedulers to use {!using} to provide an implementation
    tailored for the scheduler. *)
