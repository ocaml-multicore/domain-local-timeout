(* NOTE: We use [@poll error] and [@inline never] on functions to ensure that
   there are no safe-points where thread switches might occur during an
   operation that needs to be atomic with respect to systhread scheduling. *)

include Thread_intf
include Unix_intf

(* *)

let unimplemented _ = failwith "unimplemented"

type system = Unset | Set of (module Thread) * (module Unix)

let system_global = Atomic.make Unset

let set_system thread unix =
  Atomic.compare_and_set system_global Unset (Set (thread, unix)) |> ignore

(* *)

module Entry = struct
  type t = { time : Mtime.span; action : unit -> unit }

  let compare l r = Mtime.Span.compare l.time r.time
end

module Q = Psq.Make (Int) (Entry)

let running = Failure "timeout thread is running"

(** If you see this exception you need to make sure you don't try to register
    timeouts after the (main thread of the) domain exits. *)
let stopped = Invalid_argument "timeout thread has already been stopped"

let shared_byte = Bytes.create 1

let system_on_current_domain (module Thread : Thread) (module Unix : Unix) =
  let open struct
    type state = {
      mutable needs_wakeup : bool;
      mutable pipe_ref_count : int;
      mutable counter : int;
      mutable status : exn;
      reading : Unix.file_descr;
      writing : Unix.file_descr;
      timeouts : Q.t Atomic.t;
    }
  end in
  let s =
    let reading, writing = Unix.pipe () in
    {
      needs_wakeup = true;
      pipe_ref_count = 1;
      counter = 0;
      status = running;
      reading;
      writing;
      timeouts = Atomic.make Q.empty;
    }
  in
  let[@poll error] [@inline never] wakeup_needed_atomically s =
    let n = s.pipe_ref_count in
    s.needs_wakeup && 0 < n
    && begin
         s.needs_wakeup <- false;
         s.pipe_ref_count <- n + 1;
         true
       end
  in
  let[@poll error] [@inline never] free_pipe_atomically s =
    let n = s.pipe_ref_count in
    if 0 < n then s.pipe_ref_count <- n - 1;
    n == 1
  in
  let free_pipe s =
    if free_pipe_atomically s then begin
      Unix.close s.reading;
      Unix.close s.writing
    end
  in
  let wakeup s =
    if wakeup_needed_atomically s then begin
      let n = Unix.write s.writing shared_byte 0 1 in
      free_pipe s;
      assert (n = 1)
    end
  in
  let[@poll error] [@inline never] next_id_atomically s =
    let id = s.counter + 1 in
    s.counter <- id;
    id
  in
  let[@poll error] [@inline never] stop_atomically s =
    s.status == running
    && begin
         s.status <- stopped;
         true
       end
  in
  let[@poll error] [@inline never] running_atomically s =
    let running = s.status == running in
    s.needs_wakeup <- running;
    running
  in
  let rec timeout_thread s ts_old next =
    if running_atomically s then begin
      if ts_old == Atomic.get s.timeouts then begin
        match Unix.select [ s.reading ] [] [] next with
        | [ reading ], _, _ ->
            let n = Unix.read reading (Bytes.create 1) 0 1 in
            assert (n = 1)
        | _, _, _ -> ()
      end;
      s.needs_wakeup <- false;
      let rec loop s =
        let ts_old = Atomic.get s.timeouts in
        match Q.pop ts_old with
        | None -> timeout_thread s ts_old (-1.0)
        | Some ((_, t), ts) ->
            let elapsed = Mtime_clock.elapsed () in
            if Mtime.Span.compare t.time elapsed <= 0 then begin
              if Atomic.compare_and_set s.timeouts ts_old ts then t.action ();
              loop s
            end
            else
              let next =
                Mtime.Span.to_float_ns (Mtime.Span.abs_diff t.time elapsed)
                *. (1. /. 1_000_000_000.)
              in
              timeout_thread s ts_old next
      in
      loop s
    end
  in
  let timeout_thread s =
    begin
      match timeout_thread s Q.empty (-1.0) with
      | () -> ()
      | exception exn -> s.status <- exn
    end;
    free_pipe s;
    Atomic.set s.timeouts Q.empty
  in
  let tid = Thread.create timeout_thread s in
  let stop () =
    if stop_atomically s then wakeup s;
    Thread.join tid;
    if s.status != stopped then raise s.status
  in
  let set_timeoutf seconds action =
    match Mtime.Span.of_float_ns (seconds *. 1_000_000_000.) with
    | None ->
        invalid_arg "timeout should be between 0 to pow(2, 53) nanoseconds"
    | Some span ->
        let time = Mtime.Span.add (Mtime_clock.elapsed ()) span in
        let e' = Entry.{ time; action } in
        let id = next_id_atomically s in
        let rec insert_loop s id e' =
          let ts = Atomic.get s.timeouts in
          let ts' = Q.add id e' ts in
          if s.status != running then raise s.status
          else if not (Atomic.compare_and_set s.timeouts ts ts') then
            insert_loop s id e'
          else match Q.min ts' with Some (id', _) -> id = id' | None -> false
        in
        if insert_loop s id e' then wakeup s;
        let rec cancel () =
          let ts = Atomic.get s.timeouts in
          let ts' = Q.remove id ts in
          if not (Atomic.compare_and_set s.timeouts ts ts') then cancel ()
        in
        cancel
  in
  (stop, set_timeoutf)

(* *)

type set_timeoutf = float -> (unit -> unit) -> unit -> unit

type config =
  | Per_domain : { mutable set_timeoutf : set_timeoutf } -> config
  | Per_thread : {
      mutable set_timeoutf : set_timeoutf;
      self : unit -> 'handle;
      id : 'handle -> int;
      id_to_set_timeoutf : set_timeoutf Thread_table.t;
    }
      -> config

let try_system = ref unimplemented
let default seconds action = !try_system seconds action
let key = Domain.DLS.new_key @@ fun () -> Per_domain { set_timeoutf = default }

let[@poll error] [@inline never] update_set_timeoutf_atomically s set_timeoutf =
  match s with
  | Per_domain r ->
      let current = r.set_timeoutf in
      if current == default then begin
        r.set_timeoutf <- set_timeoutf;
        set_timeoutf
      end
      else current
  | Per_thread r ->
      let current = r.set_timeoutf in
      if current == default then begin
        r.set_timeoutf <- set_timeoutf;
        set_timeoutf
      end
      else current

let () =
  try_system :=
    fun seconds action ->
      match Atomic.get system_global with
      | Unset -> failwith "Domain_local_timeout.set_timeoutf not implemented"
      | Set (thread, unix) ->
          let stop, set_timeoutf_new = system_on_current_domain thread unix in
          let set_timeoutf =
            update_set_timeoutf_atomically (Domain.DLS.get key) set_timeoutf_new
          in
          if set_timeoutf != set_timeoutf_new then stop ()
          else Domain.at_exit stop;
          set_timeoutf seconds action

(* *)

let set_timeoutf seconds action =
  match Domain.DLS.get key with
  | Per_domain r -> r.set_timeoutf seconds action
  | Per_thread r -> begin
      match Thread_table.find r.id_to_set_timeoutf (r.id (r.self ())) with
      | set_timeoutf -> set_timeoutf seconds action
      | exception Not_found -> r.set_timeoutf seconds action
    end

(* *)

let using ~set_timeoutf ~while_running =
  match Domain.DLS.get key with
  | Per_domain r ->
      let previous = r.set_timeoutf in
      r.set_timeoutf <- set_timeoutf;
      Fun.protect while_running ~finally:(fun () -> r.set_timeoutf <- previous)
  | Per_thread r ->
      let id = r.id (r.self ()) in
      Thread_table.add r.id_to_set_timeoutf id set_timeoutf;
      Fun.protect while_running ~finally:(fun () ->
          Thread_table.remove r.id_to_set_timeoutf id)

(* *)

let per_thread (module Thread : Thread) =
  match Domain.DLS.get key with
  | Per_thread _ ->
      failwith
        "Domain_local_timeout: per_thread called twice on a single domain"
  | Per_domain { set_timeoutf } ->
      let open Thread in
      let id_to_set_timeoutf = Thread_table.create () in
      Domain.DLS.set key
        (Per_thread { set_timeoutf; self; id; id_to_set_timeoutf })
