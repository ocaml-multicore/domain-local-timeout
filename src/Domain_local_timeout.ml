include Thread_intf
include Unix_intf

(* *)

let unimplemented _ = failwith "unimplemented"

let system_unimplemented =
  let module Thread = struct
    type t

    let self = unimplemented
    let id = unimplemented
    let create = unimplemented
    let join = unimplemented
  end in
  ( ((module Thread) : thread),
    let module Unix = struct
      type file_descr

      let gettimeofday = unimplemented
      let read = unimplemented
      let write = unimplemented
      let pipe ?cloexec:_ = unimplemented
      let select = unimplemented
    end in
    ((module Unix) : unix) )

let system_global = Atomic.make system_unimplemented

let set_system thread unix =
  Atomic.compare_and_set system_global system_unimplemented (thread, unix)
  |> ignore

let has_system () = Atomic.get system_global != system_unimplemented

(* *)

type entry = { time : float; mutable action : unit -> unit }

let system_on_current_domain () =
  let ((module Thread) : thread), ((module Unix) : unix) =
    Atomic.get system_global
  in
  (* TODO: Efficient priority queue. *)
  let running = ref true in
  let needs_wakeup = ref true in
  let reading, writing = Unix.pipe () in
  let wakeup () =
    if !needs_wakeup then begin
      needs_wakeup := false;
      let n = Unix.write writing (Bytes.create 1) 0 1 in
      assert (n = 1)
    end
  in
  let timeouts = Atomic.make [] in
  let rec timeout_thread next =
    if !running then begin
      needs_wakeup := true;
      (match Unix.select [ reading ] [] [] next with
      | [ reading ], _, _ ->
          let n = Unix.read reading (Bytes.create 1) 0 1 in
          assert (n = 1)
      | _, _, _ -> ());
      let rec wakeup_loop () =
        match Atomic.get timeouts with
        | [] -> ()
        | t :: ts as tts ->
            if t.time <= Unix.gettimeofday () then begin
              if Atomic.compare_and_set timeouts tts ts then begin
                let action = t.action in
                t.action <- Fun.id;
                action ()
              end;
              wakeup_loop ()
            end
      in
      wakeup_loop ();
      match Atomic.get timeouts with
      | [] -> timeout_thread (-1.0)
      | t :: _ ->
          timeout_thread (Float.max 0.0 (t.time -. Unix.gettimeofday ()))
    end
  in
  let tid = Thread.create timeout_thread (-1.0) in
  let stop () =
    running := false;
    wakeup ();
    Thread.join tid
  in
  let set_timeoutf seconds action =
    let time = Unix.gettimeofday () +. seconds in
    let e' = { time; action } in
    let[@tail_mod_cons] rec insert = function
      | [] -> [ e' ]
      | e :: es as ees ->
          if e.time <= e'.time then e :: insert es else e' :: ees
    in
    let rec insert_loop () =
      let ts = Atomic.get timeouts in
      let ts' = insert ts in
      if not (Atomic.compare_and_set timeouts ts ts') then insert_loop ()
      else match ts' with e :: _ -> e == e' | _ -> false
    in
    if insert_loop () then wakeup ();
    let[@tail_mod_cons] rec remove e' = function
      | [] -> []
      | e :: es -> if e == e' then es else e :: remove e' es
    in
    let rec cancel () =
      e'.action <- Fun.id;
      let ts = Atomic.get timeouts in
      let ts' = remove e' ts in
      if not (Atomic.compare_and_set timeouts ts ts') then cancel ()
    in
    cancel
  in
  Domain.at_exit stop;
  set_timeoutf

(* *)

(* TODO: Per thread *)

let try_system = ref unimplemented
let key = Domain.DLS.new_key @@ fun () -> !try_system

let () =
  try_system :=
    fun seconds action ->
      if not (has_system ()) then
        failwith "Domain_local_timeout.set_timeoutf not implemented"
      else
        let set_timeoutf = system_on_current_domain () in
        Domain.DLS.set key set_timeoutf;
        set_timeoutf seconds action

(* *)

type cancel = unit -> unit

let set_timeoutf seconds action = Domain.DLS.get key seconds action

(* *)

let using ~set_timeoutf:current ~while_running =
  let previous = Domain.DLS.get key in
  Domain.DLS.set key current;
  Fun.protect while_running ~finally:(fun () -> Domain.DLS.set key previous)
