open Async_io.Std

let create_sock port =
  let sock_addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock sock_addr;
  Unix.listen sock 256;
  sock
;;

let echo_loop sock =
  let buf = Bytes.create 1024 in
  try
    let rec loop sock buf =
      let count = Async_unix.Fd.read sock buf ~pos:0 ~len:1024 in
      if count > 0
      then (
        let (_ : int) = Async_unix.Fd.write sock buf ~pos:0 ~len:count in
        loop sock buf)
    in
    loop sock buf;
    Async_unix.Sock.close sock
  with
  | _ -> Async_unix.Sock.close sock
;;

let main () =
  let port = int_of_string Sys.argv.(1) in
  let sock = create_sock port in
  let promise = Promise.create () in
  (try
     let rec accept_loop sock =
       let client_sock, _addr = Async_unix.Sock.accept sock in
       Task.spawn (fun () -> echo_loop client_sock);
       accept_loop sock
     in
     accept_loop sock
   with
  | exn ->
    let bt = Printexc.get_raw_backtrace () in
    Async_unix.Sock.close sock;
    Promise.fail promise exn bt);
  Promise.read promise
;;

let () = Async_io.run main
