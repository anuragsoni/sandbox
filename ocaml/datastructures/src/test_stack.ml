open Vessel

let stack = Threadsafe_stack.create ()

let rec producer n =
  if n = 0
  then ()
  else (
    Threadsafe_stack.push stack n;
    Format.printf "Pushing: %d\n%!" n;
    producer (n - 1))
;;

let rec consumer n =
  if n = 0
  then ()
  else (
    let v = Threadsafe_stack.pop stack in
    Format.printf "Consumed: %d\n%!" v;
    consumer (n - 1))
;;

let main () =
  let producer = Domain.spawn (fun () -> producer 10) in
  let consumer = Domain.spawn (fun () -> consumer 10) in
  Domain.join producer;
  Domain.join consumer
;;

let () = main ()
