type 'a t =
  { queue : 'a Queue.t
  ; mutex : Mutex.t
  ; condvar : Condition.t
  }

let create () =
  { queue = Queue.create (); mutex = Mutex.create (); condvar = Condition.create () }
;;

let pop t =
  Mutex.lock t.mutex;
  while Queue.is_empty t.queue do
    Condition.wait t.condvar t.mutex
  done;
  let v = Queue.pop t.queue in
  Mutex.unlock t.mutex;
  v
;;

let try_push t v =
  let locked = Mutex.try_lock t.mutex in
  if locked
  then (
    let was_empty = Queue.is_empty t.queue in
    Queue.push v t.queue;
    Mutex.unlock t.mutex;
    if was_empty then Condition.signal t.condvar;
    true)
  else false
;;

let try_pop t =
  let locked = Mutex.try_lock t.mutex in
  if locked
  then
    if Queue.is_empty t.queue
    then (
      Mutex.unlock t.mutex;
      None)
    else (
      let v = Queue.pop t.queue in
      Mutex.unlock t.mutex;
      Some v)
  else None
;;

let push t v =
  Mutex.lock t.mutex;
  let was_empty = Queue.is_empty t.queue in
  Queue.push v t.queue;
  Mutex.unlock t.mutex;
  if was_empty then Condition.signal t.condvar
;;
