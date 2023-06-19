let () = Domain_local_timeout.set_system (module Thread) (module Unix)

let test_action_raises () =
  let open struct
    exception Expected
  end in
  let domain =
    Domain.spawn @@ fun () ->
    let (_cancel : unit -> unit) =
      Domain_local_timeout.set_timeoutf 0.0 @@ fun () -> raise Expected
    in
    Unix.sleepf 0.1;
    Alcotest.check_raises "set_timeoutf raises" Expected (fun () ->
        let _cancel : unit -> unit =
          Domain_local_timeout.set_timeoutf 0.1 Fun.id
        in
        ())
  in
  Alcotest.check_raises "domain raises" Expected (fun () -> Domain.join domain)

let sleepf seconds =
  let t = Domain_local_await.prepare_for_await () in
  let cancel = Domain_local_timeout.set_timeoutf seconds t.release in
  try t.await ()
  with exn ->
    cancel ();
    raise exn

let test_two_timeouts () =
  let result = ref "" in
  let cancel_there =
    Domain_local_timeout.set_timeoutf 0.2 @@ fun () ->
    result := !result ^ "there!"
  in
  cancel_there ();
  let cancel_world =
    Domain_local_timeout.set_timeoutf 0.1 @@ fun () ->
    result := !result ^ "world!"
  in
  result := !result ^ "Hello, ";
  sleepf 0.3;
  cancel_world ();
  Alcotest.check' ~msg:"result is as expected" Alcotest.string
    ~expected:"Hello, world!" ~actual:!result

let test_per_thread () =
  Domain_local_timeout.per_thread (module Thread);
  let threads =
    Array.init 10 @@ fun i ->
    ()
    |> Thread.create @@ fun () ->
       if i land 1 == 0 then
         Domain_local_timeout.using
           ~set_timeoutf:(fun _seconds action ->
             action ();
             Fun.id)
           ~while_running:(fun () -> sleepf 10.0)
       else sleepf 0.1
  in
  Array.iter Thread.join threads

let () =
  [
    (if Domain.is_real then
       [
         ("action raises", [ Alcotest.test_case "" `Quick test_action_raises ]);
       ]
     else []);
    [ ("two timeouts", [ Alcotest.test_case "" `Quick test_two_timeouts ]) ];
    [ ("per thread", [ Alcotest.test_case "" `Quick test_per_thread ]) ];
  ]
  |> List.concat
  |> Alcotest.run "Domain_local_timeout"
