type 'a t =
  { stack : 'a Stack.t
  ; mutex : Mutex.t
  ; condition : Condition.t
  }

let create () =
  { stack = Stack.create (); mutex = Mutex.create (); condition = Condition.create () }
;;

let push t data =
  Mutex.lock t.mutex;
  let was_empty = Stack.is_empty t.stack in
  Stack.push data t.stack;
  if was_empty then Condition.signal t.condition;
  Mutex.unlock t.mutex
;;

let pop t =
  Mutex.lock t.mutex;
  while Stack.is_empty t.stack do
    Condition.wait t.condition t.mutex
  done;
  let res = Stack.pop t.stack in
  Mutex.unlock t.mutex;
  res
;;

let length t =
  Mutex.lock t.mutex;
  let length = Stack.length t.stack in
  Mutex.unlock t.mutex;
  length
;;
