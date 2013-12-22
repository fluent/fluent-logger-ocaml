open OUnit

let setup_test_server () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt socket Unix.TCP_NODELAY true;
  Unix.set_close_on_exec socket;
  Unix.bind socket (Unix.ADDR_INET(Unix.inet_addr_any, 0));
  Unix.listen socket 50;
  let ev_ch = Event.new_channel () in
  ignore (
    Thread.create (fun _ ->
      let (client_socket, sockaddr) = Unix.accept socket in
      ignore (Event.sync (Event.send ev_ch client_socket));
      let in_ch = Unix.in_channel_of_descr client_socket in
      let rec _loop () =
        let buf = String.create 1024 in
        let len = input in_ch buf 0 1024 in
        if len = 0 then (
          close_in in_ch
        )
        else (
          print_endline (String.sub buf 0 len);
          _loop ()
        )
        in
      _loop ()
    ) ()
  );
  match Unix.getsockname socket with
  | Unix.ADDR_INET (_, port) ->
      (socket, port, (fun () -> Event.sync (Event.receive ev_ch)))
  | _ -> failwith "unexpected socket domain"

let test_inet_sender_write () =
  (* setup *)
  let (socket, port, recv_client_socket) = setup_test_server () in
  let sender = Inet_sender.create ~port:port () in
  (* first msg *)
  let msg = "helloworld" in
  let msg_len = String.length msg in
  assert_equal (Some msg_len) (Inet_sender.write sender msg 0 msg_len);
  (* second msg *)
  let msg = "abracadabra" in
  let msg_len = String.length msg in
  assert_equal (Some msg_len) (Inet_sender.write sender msg 0 msg_len);
  (* close server and then third msg *)
  let client_socket = recv_client_socket () in
  Unix.close client_socket;
  Unix.close socket;
  let msg = "foobar" in
  let msg_len = String.length msg in
  assert_equal None (Inet_sender.write sender msg 0 msg_len)

let suite =
  "Fluent_logger tests" >:::
    ["test_inet_sender_write" >:: test_inet_sender_write
    ]

let () =
  Printexc.record_backtrace true;
  ignore (run_test_tt_main suite)

