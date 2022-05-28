open Vessel
module Q = Threadsafe_queue

let queue = Q.create ()

let producer n =
  for i = 1 to n do
    Q.push queue i;
    Format.printf "Pushing: %d\n%!" i
  done
;;

let consumer n =
  for i = n downto 1 do
    let v = Q.pop queue in
    Format.printf "Consumed: %d\n%!" v
  done
;;

let main () =
  let producer = Domain.spawn (fun () -> producer 10) in
  let consumer = Domain.spawn (fun () -> consumer 10) in
  Domain.join producer;
  Domain.join consumer
;;

let () = main ()
