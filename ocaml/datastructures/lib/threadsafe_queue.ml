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

let push t v =
  Mutex.lock t.mutex;
  let was_empty = Queue.is_empty t.queue in
  Queue.push v t.queue;
  Mutex.unlock t.mutex;
  if was_empty then Condition.signal t.condvar
;;
