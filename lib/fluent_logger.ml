module type SENDER_TYPE =
  sig
    type t
    val close : t -> unit
    val write : t -> string -> int -> int -> int option
  end
 
module Make = functor (Elm : SENDER_TYPE) -> struct

  type t = {
    sender:Elm.t;
    buf:Buffer.t;
    mutable buf_pos:int;
  }

  let create_with_sender ?(bufsize:int = 8 * 1024 * 1024) sender = { 
      sender=sender;
      buf=Buffer.create bufsize;
      buf_pos=0;
    }

  let close logger = Elm.close logger.sender

  let flush_buf logger =
    let buflen = Buffer.length logger.buf in
    let rec _write () =
      let result = Elm.write logger.sender
                  (Buffer.sub logger.buf logger.buf_pos buflen)
                  logger.buf_pos
                  (buflen - logger.buf_pos) in
      match result with
      | Some len -> (
        logger.buf_pos <- logger.buf_pos + len;
        if logger.buf_pos >= buflen then (
          logger.buf_pos <- 0;
          Buffer.clear logger.buf;
          true
        )
        else _write ()
      )
      | None -> (
        prerr_endline "flush_buf: write error";
        Elm.close logger.sender;
        false
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

end

module Inet = Make(Inet_sender)

let create_for_inet ?(bufsize:int = 8 * 1024 * 1024) ?(conn_timeout:int = 3)
  ?(host:string = "localhost") ?(port:int = 24224) () = 
  let sender = Inet_sender.create_for_inet ~conn_timeout ~host ~port () in
  Inet.create_with_sender ~bufsize sender

let create = create_for_inet
