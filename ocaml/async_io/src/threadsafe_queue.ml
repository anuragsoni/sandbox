type t =
  { queue : Optional_thunk.t Queue.t
  ; mutex : Mutex.t
  ; not_empty : Condition.t
  }

let create () =
  let queue = Queue.create () in
  let mutex = Mutex.create () in
  let not_empty = Condition.create () in
  { queue; mutex; not_empty }
;;

let try_pop t =
  if Mutex.try_lock t.mutex
  then (
    let v = if Queue.is_empty t.queue then Optional_thunk.none else Queue.pop t.queue in
    Mutex.unlock t.mutex;
    v)
  else Optional_thunk.none
;;

let pop t =
  Mutex.lock t.mutex;
  while Queue.is_empty t.queue do
    Condition.wait t.not_empty t.mutex
  done;
  let v = Queue.pop t.queue in
  Mutex.unlock t.mutex;
  Optional_thunk.unsafe_fn v
;;

let try_push t fn =
  let fn = Optional_thunk.some fn in
  if Mutex.try_lock t.mutex
  then (
    let was_empty = Queue.is_empty t.queue in
    Queue.push fn t.queue;
    if was_empty then Condition.signal t.not_empty;
    Mutex.unlock t.mutex;
    true)
  else false
;;

let push t fn =
  let fn = Optional_thunk.some fn in
  Mutex.lock t.mutex;
  let was_empty = Queue.is_empty t.queue in
  Queue.push fn t.queue;
  if was_empty then Condition.signal t.not_empty;
  Mutex.unlock t.mutex
;;
