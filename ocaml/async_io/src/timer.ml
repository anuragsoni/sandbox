type _ Effect.t += Wakup_at : Mtime.t -> unit Effect.t

let sleep span =
  let now = Mtime_clock.now () in
  let at =
    match Mtime.add_span now span with
    | None -> failwith "Sleep duration will lead to an overflow"
    | Some t -> t
  in
  Effect.perform (Wakup_at at)
;;

module K = struct
  type t = int64

  let compare a b = Int64.compare a b
end

module Cont = struct
  type t =
    { cont : (unit, unit) Effect.Deep.continuation
    ; at : int64
    }

  let compare a b = Int64.compare a.at b.at
end

module Q = Psq.Make (K) (Cont)

type t = { mutable continuations : Q.t }

let create () = { continuations = Q.empty }

let add t ~at cont =
  let at = Mtime.to_uint64_ns at in
  let cont = { Cont.cont; at } in
  t.continuations <- Q.add at cont t.continuations
;;

let next_wakeup_at ~now t =
  match Q.min t.continuations with
  | None -> Poll.Timeout.after 50_000_000L
  | Some (at, _) ->
    let span = Mtime.span now (Mtime.of_uint64_ns at) in
    Poll.Timeout.after (Mtime.Span.to_uint64_ns span)
;;

let rec advance_timer' t ~now ~push =
  match Q.min t.continuations with
  | Some (span, cont) ->
    if span <= now
    then (
      push cont.cont;
      t.continuations <- Q.remove span t.continuations;
      advance_timer' t ~now ~push)
  | None -> ()
;;

let advance_timer t ~now ~push =
  if not (Q.is_empty t.continuations)
  then (
    let now = Mtime.to_uint64_ns now in
    advance_timer' t ~now ~push)
;;

let has_events t = not (Q.is_empty t.continuations)
