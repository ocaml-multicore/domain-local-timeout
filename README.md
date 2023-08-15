[API reference](https://ocaml-multicore.github.io/domain-local-timeout/doc/domain-local-timeout/Domain_local_timeout/index.html)

# **domain-local-timeout** &mdash; Scheduler independent timeout

A low level mechanism intended for writing higher level libraries that need to
be able to have scheduler friendly timeouts.

A library that needs timeouts may simply call
[`set_timeoutf`](https://ocaml-multicore.github.io/domain-local-timeout/doc/domain-local-timeout/Domain_local_timeout/index.html#val-set_timeoutf).

To provide an efficient scheduler specific implementation of the mechanism,
schedulers may install an implementation by wrapping the scheduler main loop
with a call to
[`using`](https://ocaml-multicore.github.io/domain-local-timeout/doc/domain-local-timeout/Domain_local_timeout/index.html#val-using).
The implementation is then stored in a domain, and optionally thread, local
variable. The overhead that this imposes on a scheduler should be insignificant.

An application can then choose to use schedulers that provide the necessary
implementation or, for example, use the default implementation by calling
[`set_system`](https://ocaml-multicore.github.io/domain-local-timeout/doc/domain-local-timeout/Domain_local_timeout/index.html#val-set_system).

The end result is effective interoperability between schedulers and concurrent
programming libraries.

## References

DLT is used by the following libraries:

- [Kcas](https://ocaml-multicore.github.io/kcas/) uses DLT to implement
  [timeouts](https://github.com/ocaml-multicore/kcas/#timeouts).

## Example

First we require some libraries we are using:

```ocaml
# #thread
# #require "domain-local-timeout"
# #require "domain-local-await"
```

Here is how one could implement a scheduler independent and friendly way to
sleep:

```ocaml
# let sleepf seconds =
    let t = Domain_local_await.prepare_for_await () in
    let cancel = Domain_local_timeout.set_timeoutf seconds t.release in
    try t.await ()
    with exn ->
      cancel ();
      raise exn
val sleepf : float -> unit = <fun>
```

Note that the above is careful to call `cancel` in case `await` raises an
exception. That could happen when the fiber on which `sleepf` was called is
canceled, in which case it makes sense to cancel the timeout.

Let's try it:

```ocaml
# sleepf 1.0
Exception: Failure "Domain_local_timeout.set_timeoutf not implemented".
```

Oops!

To actually use domain-local-timeout we need an implementation. There is a
default implementation that uses the `Stdlib.Thread` and `Stdlib.Unix` modules,
but it is also possible to implement the facility in other ways and it is
recommended for schedulers to provide their own optimized implementations. Both
of those system modules are optional and are not provided on all platforms. For
these reasons domain-local-timeout does not directly depend on those libraries.
To use the default implementation, we need to require those libraries and tell
domain-local-timeout that it can use those system libraries:

```ocaml
# Domain_local_timeout.set_system (module Thread) (module Unix)
- : unit = ()
```

Now we are ready to try setting timeouts:

```ocaml
# let cancel =
    Domain_local_timeout.set_timeoutf 0.1 @@ fun () ->
    Printf.printf "world!\n%!"
  in
  Printf.printf "Hello, %!";
  try sleepf 0.2
  with exn ->
    cancel ();
    raise exn
Hello, world!
- : unit = ()
```

The above example first registers a timeout to print the end of the message and
then immediately prints the beginning. Finally the example sleeps a bit to wait
for the end to be printed.
