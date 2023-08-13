(** A scheduler independent timeout mechanism.

    This is designed as a low level mechanism intended for writing higher level
    libraries that need to be able to have scheduler friendly timeouts.

    A library that needs timeouts may simply call {!set_timeoutf}.

    To provide an efficient scheduler specific implementation of the mechanism,
    schedulers may install an implementation by wrapping the scheduler main loop
    with a call to {!using}.  The implementation is then stored in a domain, and
    optionally thread, local variable.  The overhead that this imposes on a
    scheduler should be insignificant.

    An application can then choose to use schedulers that provide the necessary
    implementation or, for example, use the default implementation by calling
    {!set_system}.

    The end result is effective interoperability between schedulers and
    concurrent programming libraries. *)

(** {1 Interface for timeouts} *)

val set_timeoutf : float -> (unit -> unit) -> unit -> unit
(** [let cancel = set_timeoutf seconds action] registers the [action] to be
    called after the specified time period in [seconds] has passed.  The return
    value [cancel] is an idempotent and domain safe action that can, and almost
    always should, be arranged to be called to cancel the timeout in case the
    timeout is no longer needed.

    {b NOTE}: The [action] must not raise exceptions or perform effects, should
    not block, and should usually just perform some minimal side-effect to
    e.g. unblock a fiber to do the work.  With the default implementation, in
    case an [action] raises an exception, the timeout mechanism is disabled and
    subsequent [set_timeoutf] calls will raise the exception and the domain will
    also raise the exception at exit.

    {b WARNING}: It is allowed for the given [action] to be called e.g. from
    another thread outside of the scheduler from which [set_timeoutf] was
    called.

    {b NOTE}: Out of bounds values for [seconds] may be rejected by raising an
    [Invalid_argument] exception.

    {b NOTE}: Implementations should schedule timeouts using a monotonic
    clock. *)

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

val set_system : (module Thread) -> (module Unix) -> unit
(** [set_system (module Thread) (module Unix)] sets the system libraries to use
    for a default {!set_timeoutf} implementation.  This operation has an effect
    only on first time and, if needed, should be called from application code on
    the main domain before code using timeouts is called.

    {b NOTE}: The default {!set_timeoutf} may not always be needed.  It is
    possible for schedulers to use {!using} to provide an implementation
    tailored for the scheduler. *)

(** {2 Per thread configuration} *)

val per_thread : (module Thread) -> unit
(** [per_thread (module Thread)] configures the current domain to store and
    select the timeout mechanism per systhread.  This can be called at most once
    per domain before any calls to {!set_timeoutf}.

    The reason why this is an opt-in feature is that this allows domain local
    timeout to be implemented without depending on [Thread] which also depends
    on [Unix].

    Usage:

    {[
      Domain.spawn @@ fun () ->
        Domain_local_timeout.per_thread (module Thread);

        (* ... *)

        ()
        |> Thread.create (fun () ->
           Domain_local_timeout.using
             ~set_timeoutf:set_timeoutf_for_scheduler_a
             ~while_running:scheduler_a);

        ()
        |> Thread.create (fun () ->
           Domain_local_timeout.using
             ~set_timeoutf:set_timeoutf_for_scheduler_b
             ~while_running:scheduler_b);

        (* ... *)
    ]}

    {b NOTE}: It is not necessary to use per systhread configuration on a domain
    unless you want different systhreads to use different schedulers. *)
