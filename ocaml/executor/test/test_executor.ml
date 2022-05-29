let create_sock port =
  let socket = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, port));
  Unix.listen socket 128;
  socket
;;

let client_loop client_sock _executor =
  let buf = Bytes.create 1024 in
  let rec loop () =
    let count = Unix.read client_sock buf 0 1024 in
    if count = 0
    then ()
    else (
      ignore (Unix.write client_sock buf 0 count : int);
      loop ())
  in
  loop ()
;;

let rec server_loop sock executor =
  let client_fd, _ = Unix.accept sock in
  Executor.async executor (fun () -> client_loop client_fd executor);
  server_loop sock executor
;;

let () =
  let port = int_of_string Sys.argv.(1) in
  let executor = Executor.create () in
  let sock = create_sock port in
  server_loop sock executor
;;
