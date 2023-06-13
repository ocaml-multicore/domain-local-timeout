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
  ( ((module Thread) : (module Thread)),
    let module Unix = struct
      type file_descr

      let close = unimplemented
      let read = unimplemented
      let write = unimplemented
      let pipe ?cloexec:_ = unimplemented
      let select = unimplemented
    end in
    ((module Unix) : (module Unix)) )

let system_global = Atomic.make system_unimplemented

let set_system thread unix =
  Atomic.compare_and_set system_global system_unimplemented (thread, unix)
  |> ignore

let has_system () = Atomic.get system_global != system_unimplemented

(* *)

module Entry = struct
  type t = { time : Mtime.span; action : unit -> unit }

  let compare l r = Mtime.Span.compare l.time r.time
end

module Q = Psq.Make (Int) (Entry)

let system_on_current_domain () =
  let ((module Thread) : (module Thread)), ((module Unix) : (module Unix)) =
    Atomic.get system_global
  in
  let error = ref None in
  let check () = match !error with None -> () | Some exn -> raise exn in
  let running = ref true in
  let needs_wakeup = ref true in
  let reading, writing = Unix.pipe () in
  let wakeup () =
    if !needs_wakeup && !error == None then begin
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
      let rec loop () =
        let ts_old = Atomic.get timeouts in
        match Q.pop ts_old with
        | None -> -1.0
        | Some ((_, t), ts) ->
            let elapsed = Mtime_clock.elapsed () in
            if Mtime.Span.compare t.time elapsed <= 0 then begin
              if Atomic.compare_and_set timeouts ts_old ts then t.action ();
              loop ()
            end
            else
              Mtime.Span.to_float_ns (Mtime.Span.abs_diff t.time elapsed)
              *. (1. /. 1_000_000_000.)
      in
      timeout_thread (loop ())
    end
  in
  let timeout_thread () =
    (match timeout_thread (-1.0) with
    | () -> ()
    | exception exn -> error := Some exn);
    Unix.close reading;
    Unix.close writing
  in
  let tid = Thread.create timeout_thread () in
  let stop () =
    running := false;
    wakeup ();
    Thread.join tid;
    check ()
  in
  let set_timeoutf seconds action =
    match Mtime.Span.of_float_ns (seconds *. 1_000_000_000.) with
    | None ->
        invalid_arg "timeout should be between 0 to pow(2, 53) nanoseconds"
    | Some span ->
        check ();
        let time = Mtime.Span.add (Mtime_clock.elapsed ()) span in
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

type config =
  | Per_domain : (float -> (unit -> unit) -> unit -> unit) -> config
  | Per_thread : {
      mutable default : float -> (unit -> unit) -> unit -> unit;
      self : unit -> 'handle;
      id : 'handle -> int;
      id_to_set_timeoutf :
        (float -> (unit -> unit) -> unit -> unit) Thread_table.t;
    }
      -> config

let try_system = ref unimplemented
let key = Domain.DLS.new_key @@ fun () -> Per_domain !try_system

let () =
  try_system :=
    fun seconds action ->
      if not (has_system ()) then
        failwith "Domain_local_timeout.set_timeoutf not implemented"
      else
        let set_timeoutf = system_on_current_domain () in
        match Domain.DLS.get key with
        | Per_domain _ ->
            Domain.DLS.set key (Per_domain set_timeoutf);
            set_timeoutf seconds action
        | Per_thread r ->
            if r.default == !try_system then r.default <- set_timeoutf;
            r.default seconds action

(* *)

let set_timeoutf seconds action =
  match Domain.DLS.get key with
  | Per_domain set_timeoutf -> set_timeoutf seconds action
  | Per_thread r -> (
      match Thread_table.find r.id_to_set_timeoutf (r.id (r.self ())) with
      | set_timeoutf -> set_timeoutf seconds action
      | exception Not_found -> r.default seconds action)

(* *)

let using ~set_timeoutf ~while_running =
  match Domain.DLS.get key with
  | Per_domain _ as previous ->
      Domain.DLS.set key (Per_domain set_timeoutf);
      Fun.protect while_running ~finally:(fun () -> Domain.DLS.set key previous)
  | Per_thread r ->
      let id = r.id (r.self ()) in
      Thread_table.add r.id_to_set_timeoutf id set_timeoutf;
      Fun.protect while_running ~finally:(fun () ->
          Thread_table.remove r.id_to_set_timeoutf id)

(* *)

let per_thread ((module Thread) : (module Thread)) =
  match Domain.DLS.get key with
  | Per_thread _ ->
      failwith
        "Domain_local_timeout: per_thread called twice on a single domain"
  | Per_domain default ->
      let open Thread in
      let id_to_set_timeoutf = Thread_table.create () in
      Domain.DLS.set key (Per_thread { default; self; id; id_to_set_timeoutf })
