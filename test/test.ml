let () = Domain_local_timeout.set_system (module Thread) (module Unix)

let sleepf seconds =
  let t = Domain_local_await.prepare_for_await () in
  let cancel = Domain_local_timeout.set_timeoutf seconds t.release in
  try t.await ()
  with exn ->
    (cancel :> unit -> unit) ();
    raise exn

let () =
  let result = ref "" in
  let cancel_there =
    Domain_local_timeout.set_timeoutf 0.2 @@ fun () ->
    result := !result ^ "there!"
  in
  (cancel_there :> unit -> unit) ();
  let cancel_world =
    Domain_local_timeout.set_timeoutf 0.1 @@ fun () ->
    result := !result ^ "world!"
  in
  result := !result ^ "Hello, ";
  sleepf 0.3;
  (cancel_world :> unit -> unit) ();
  assert (!result = "Hello, world!")

let () = Printf.printf "Test suite OK\n%!"
