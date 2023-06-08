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

module Entry = struct
  type t = { time : float; action : unit -> unit }

  let compare l r = Float.compare l.time r.time
end

module Q = Psq.Make (Int) (Entry)

let system_on_current_domain () =
  let ((module Thread) : thread), ((module Unix) : unix) =
    Atomic.get system_global
  in
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
  let counter = ref 0 in
  let next_id () =
    let id = !counter + 1 in
    counter := id;
    id
  in
  let timeouts = Atomic.make Q.empty in
  let rec timeout_thread next =
    if !running then begin
      needs_wakeup := true;
      (match Unix.select [ reading ] [] [] next with
      | [ reading ], _, _ ->
          let n = Unix.read reading (Bytes.create 1) 0 1 in
          assert (n = 1)
      | _, _, _ -> ());
      let rec wakeup_loop () =
        let ts_old = Atomic.get timeouts in
        match Q.pop ts_old with
        | None -> ()
        | Some ((_, t), ts) ->
            if t.time <= Unix.gettimeofday () then begin
              if Atomic.compare_and_set timeouts ts_old ts then t.action ();
              wakeup_loop ()
            end
      in
      wakeup_loop ();
      match Q.min (Atomic.get timeouts) with
      | None -> timeout_thread (-1.0)
      | Some (_, t) ->
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
    let e' = Entry.{ time; action } in
    let id = next_id () in
    let rec insert_loop () =
      let ts = Atomic.get timeouts in
      let ts' = Q.add id e' ts in
      if not (Atomic.compare_and_set timeouts ts ts') then insert_loop ()
      else match Q.min ts' with Some (id', _) -> id = id' | None -> false
    in
    if insert_loop () then wakeup ();
    let rec cancel () =
      let ts = Atomic.get timeouts in
      let ts' = Q.remove id ts in
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
