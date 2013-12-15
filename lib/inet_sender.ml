type dst_info = INET of string * int | UNIX of string

type t = {
  dst_info:dst_info;
  mutable socket:Unix.file_descr option;
  mutable last_conn_err_time:int;
  mutable conn_err_count:int;
  conn_timeout:int;
  dummy_one_byte:string;
}

let create_for_inet
  ?(conn_timeout:int = 3)
  ?(host:string = "localhost")
  ?(port:int = 24224) () = { 
    dst_info=INET(host, port);
    socket=None;
    last_conn_err_time=0;
    conn_err_count=0;
    conn_timeout=conn_timeout;
    dummy_one_byte=String.create 1;
  }

let create = create_for_inet

let print_unix_error label e fn param =
  Printf.eprintf "%s: [%s] [%s] [%s]\n" label (Unix.error_message e) fn param

let connect sender = 
  match sender.dst_info with
  | INET(host, port) -> (
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt socket Unix.TCP_NODELAY true;
    let server_addresses =
      Array.to_list((Unix.gethostbyname host).Unix.h_addr_list) in
    try (
      ignore (
        List.find (
          fun addr -> 
            try (
              Unix.connect socket (Unix.ADDR_INET(addr, port));
              true
            )
            with Unix.Unix_error (e, fn, p) -> (
              print_unix_error "connect error" e fn p;
              false
            )
        ) server_addresses
      );
      sender.socket <- Some socket
    )
    with Not_found -> (
      sender.last_conn_err_time <- int_of_float (Unix.time ());
      sender.conn_err_count <- sender.conn_err_count + 1;
      sender.socket <- None
    )
  )
  | UNIX _ -> failwith "not implemented yet"

let connect_if_needed sender =
  match sender.socket with
  | Some _ -> ()
  | None -> connect sender

let close sender =
  match sender.socket with
  | Some s -> (
      try Unix.close s
      with Unix.Unix_error (e, fn, p) ->
        print_unix_error "close error" e fn p
    );
    sender.socket <- None
  | None -> ()

let update_conn sender =
  match sender.socket with
  | Some socket -> (
    Unix.set_nonblock socket;
    let connected =
      try Unix.read socket sender.dummy_one_byte 0 1 != 0
      with 
      | Unix.Unix_error (Unix.EAGAIN, _, _) -> true
      | _ -> false
    in (
      Unix.clear_nonblock socket;
      if not connected then close sender
    )
  )
  | None -> ()

let write sender buf start offset =
  update_conn sender;
  connect_if_needed sender;
  match sender.socket with
  | Some socket -> (
    try Some (Unix.single_write socket buf start offset)
    with Unix.Unix_error (e, fn, p) -> (
      print_unix_error "write error" e fn p;
      close sender;
      None
    )
  )
  | None -> (
    prerr_endline "write error: not connected";
    close sender;
    None
  )

