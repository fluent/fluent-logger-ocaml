type t = {
  sender:Stream_sender.t;
  buf:Buffer.t;
  mutable buf_pos:int;
}

let default_bufsize = 8 * 1024 * 1024
let default_host = "localhost"
let default_port = 24224
let default_conn_timeout = 3

let close logger = Stream_sender.close logger.sender

let flush_buf logger =
  let buflen = Buffer.length logger.buf in
  let rec _write () =
    if logger.buf_pos >= buflen then (
      logger.buf_pos <- 0;
      Buffer.clear logger.buf;
      true
    )
    else (
      let result = Stream_sender.write logger.sender
                    (Buffer.contents logger.buf)
                    logger.buf_pos
                    (buflen - logger.buf_pos) in
      match result with
      | Some len -> (
        logger.buf_pos <- logger.buf_pos + len;
        _write ()
      )
      | None -> (
        prerr_endline "flush_buf: write error";
        Stream_sender.close logger.sender;
        false
      )
    )
  in
  _write ()

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

let init sender bufsize =
  {
    sender=sender;
    buf=Buffer.create bufsize;
    buf_pos=0;
  }

let create_with_sender ?(bufsize = default_bufsize)
  ?(conn_timeout = default_conn_timeout) sender = init sender bufsize

let create_for_inet ?(bufsize = default_bufsize)
  ?(conn_timeout = default_conn_timeout)
  ?(host = default_host) ?(port = default_port) () =
  let sender =
    Stream_sender.create (Stream_sender.INET(host, port)) conn_timeout in
  init sender bufsize

let create_for_unix ?(bufsize = default_bufsize)
  ?(conn_timeout = default_conn_timeout) path =
  let sender =
    Stream_sender.create (Stream_sender.UNIX(path)) conn_timeout in
  init sender bufsize

let create = create_for_inet
