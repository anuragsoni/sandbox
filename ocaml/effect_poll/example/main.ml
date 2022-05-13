let create_sock port =
  let sock_addr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock sock_addr;
  Unix.listen sock 256;
  Unix.set_nonblock sock;
  sock
;;

let close_sock sock =
  try Unix.shutdown sock Unix.SHUTDOWN_ALL with
  | _ ->
    ();
    Unix.close sock
;;

let echo_loop sock =
  let buf = Bytes.create 1024 in
  try
    let rec loop sock buf =
      match Effect_poll.read sock buf ~pos:0 ~len:1024 with
      | `Eof -> ()
      | `Ok count ->
        (match Effect_poll.write sock buf ~pos:0 ~len:count with
        | `Eof -> ()
        | `Ok _ -> loop sock buf)
    in
    loop sock buf;
    close_sock sock
  with
  | _ -> close_sock sock
;;

let main () =
  let port = int_of_string Sys.argv.(1) in
  let sock = create_sock port in
  Printf.printf "Listening on http://localhost:%d\n%!" port;
  try
    let rec accept_loop sock =
      let client_sock, _client_addr = Effect_poll.accept sock in
      Effect_poll.fork (fun () -> echo_loop client_sock);
      accept_loop sock
    in
    accept_loop sock
  with
  | exn ->
    Printf.printf "%s\n%!" (Printexc.to_string exn);
    close_sock sock
;;

let () = Effect_poll.run main
