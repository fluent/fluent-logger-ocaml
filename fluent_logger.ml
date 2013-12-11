type dst_info = INET of string * int | UNIX of string

type t = {
  dst_info:dst_info;
  mutable socket:Unix.file_descr option;
  buf:Buffer.t;
  mutable buf_pos:int;
  mutable last_conn_err_time:int;
  mutable conn_err_count:int;
  conn_timeout:int;
  dummy_one_byte:string;
}

let create_for_inet
  ?(bufsize:int = 8 * 1024 * 1024)
  ?(conn_timeout:int = 3)
  ?(host:string = "localhost")
  ?(port:int = 24224) () = { 
    dst_info=INET(host, port);
    socket=None;
    buf=Buffer.create bufsize;
    buf_pos=0;
    last_conn_err_time=0;
    conn_err_count=0;
    conn_timeout=conn_timeout;
    dummy_one_byte=String.create 1;
  }

let create = create_for_inet

let print_unix_error label e fn param =
  Printf.eprintf "%s: [%s] [%s] [%s]\n" label (Unix.error_message e) fn param

let connect logger = 
  match logger.dst_info with
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
      logger.socket <- Some socket
    )
    with Not_found -> (
      logger.last_conn_err_time <- int_of_float (Unix.time ());
      logger.conn_err_count <- logger.conn_err_count + 1;
      logger.socket <- None
    )
  )
  | UNIX _ -> failwith "not implemented yet"

let connect_if_needed logger =
  match logger.socket with
  | Some _ -> ()
  | None -> connect logger

let close logger =
  match logger.socket with
  | Some s -> (
      try Unix.close s
      with Unix.Unix_error (e, fn, p) ->
        print_unix_error "close error" e fn p
    );
    logger.socket <- None
  | None -> ()

let flush_buf logger =
  connect_if_needed logger;
  match logger.socket with
  | Some socket -> (
    try 
      let buflen = Buffer.length logger.buf in
      let rec _write () =
        Unix.set_nonblock socket;
        let connected =
          try Unix.read socket logger.dummy_one_byte 0 1 != 0
          with Unix.Unix_error (Unix.EAGAIN, _, _) -> true
        in
        if connected then (
          Unix.clear_nonblock socket;
          let len = Unix.write socket
                      (Buffer.sub logger.buf logger.buf_pos buflen)
                      logger.buf_pos
                      (buflen - logger.buf_pos) in
          logger.buf_pos <- logger.buf_pos + len;
          if logger.buf_pos >= buflen then (
            logger.buf_pos <- 0;
            Buffer.clear logger.buf;
            true
          )
          else _write ()
        )
        else (
          prerr_endline "post: disconnected";
          Unix.clear_nonblock socket;
          close logger;
          false
        )
      in
      _write ()
    with Unix.Unix_error (e, fn, p) -> (
      print_unix_error "post error" e fn p;
      close logger;
      false
    )
  )
  | None -> false

let of_string s = `FixRaw (ExtString.String.explode s)

let uint8_of_int i = `Uint8 i

let uint16_of_int i = `Uint16 i

let uint32_of_int i = `Uint32 (Int64.of_int i)

let uint64_of_int i = `Uint64 (Int64.of_int i)

let int8_of_int i = `Int8 i

let int16_of_int i = `Int16 i

let int32_of_int i = `Int32 (Int64.of_int i)

let int64_of_int i = `Int64 (Int64.of_int i)

let of_float f = `Float f

let of_double d = `Double d

let post_with_time logger tag record time =
  let packed = 
    let open Msgpack in
    let tag = of_string tag in
    Serialize.serialize_string
      (`FixArray [tag; `Uint32 time; record]) in
  Buffer.add_string logger.buf packed;
  flush_buf logger

let post logger tag record =
  post_with_time logger tag record (Int64.of_float (Unix.time ()))

let release logger =
  if not (flush_buf logger) then
    prerr_endline "release post failed";
  close logger

