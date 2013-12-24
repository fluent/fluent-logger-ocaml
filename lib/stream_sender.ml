module U = Unix

type dst_info = INET of string * int | UNIX of string

class stream_sender conn_timeout dst_info = object(self)

val dst_info = dst_info
val conn_timeout = conn_timeout
val mutable socket = None
val mutable last_conn_err_time = None
val mutable conn_err_count = 0
val dummy_one_byte = String.create 1

method private print_unix_error label e fn param =
  Printf.eprintf "%s: [%s] [%s] [%s]\n" label (U.error_message e) fn param

method private connect = 
  let s =
    U.socket
      (match dst_info with INET _ -> U.PF_INET | UNIX _ -> U.PF_UNIX)
      U.SOCK_STREAM 0 in
  let conn_success () =
    last_conn_err_time <- None;
    conn_err_count <- 0;
    socket <- Some s in
  let conn_failed () =
    last_conn_err_time <- Some (int_of_float (U.time ()));
    conn_err_count <- conn_err_count + 1;
    socket <- None in
  match dst_info with
  | INET(host, port) -> (
    U.setsockopt s U.TCP_NODELAY true;
    U.set_close_on_exec s;
    let server_addresses =
      Array.to_list((U.gethostbyname host).U.h_addr_list) in
    try (
      ignore (
        List.find (
          fun addr ->
            try (
              U.connect s (U.ADDR_INET(addr, port));
              true
            )
            with U.Unix_error (e, fn, p) -> (
              self#print_unix_error "connect error" e fn p;
              false
            )
        ) server_addresses
      );
      conn_success ()
    )
    with Not_found -> conn_failed ()
  )
  | UNIX(path) -> 
    try (
      U.connect s (U.ADDR_UNIX(path));
      conn_success ()
    )
    with U.Unix_error (e, fn, p) -> (
      self#print_unix_error "connect error" e fn p;
      conn_failed ()
    )

method private connect_if_needed =
  match socket with
  | Some _ -> ()
  | None -> (
    match last_conn_err_time with
    | Some tm -> (
        let now = int_of_float (U.time ()) in
        let interval =
          int_of_float (2.0 ** (float_of_int conn_err_count)) in
        let max_conn_retain_interval = 60 in
        let interval =
          if interval < max_conn_retain_interval then interval
          else max_conn_retain_interval in
        if tm < now - interval then self#connect
    )
    | None -> self#connect
  )

method close =
  match socket with
  | Some s -> (
      try U.close s
      with U.Unix_error (e, fn, p) ->
        self#print_unix_error "close error" e fn p
    );
    socket <- None
  | None -> ()

method private update_conn =
  match socket with
  | Some socket -> (
    U.set_nonblock socket;
    let connected =
      try U.read socket dummy_one_byte 0 1 != 0
      with 
      | U.Unix_error (U.EAGAIN, _, _) -> true
      | _ -> false
    in (
      U.clear_nonblock socket;
      if not connected then self#close
    )
  )
  | None -> ()

method write buf start offset =
  self#update_conn;
  self#connect_if_needed;
  match socket with
  | Some s -> (
    try Some (U.single_write s buf start offset)
    with U.Unix_error (e, fn, p) -> (
      self#print_unix_error "write error" e fn p;
      self#close;
      None
    )
  )
  | None -> (
    prerr_endline "write error: not connected";
    self#close;
    None
  )
end
