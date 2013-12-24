type dst_info = INET of string * int | UNIX of string
class stream_sender :
  'a ->
  dst_info ->
  object
    method close : unit
    method write : string -> int -> int -> int option
  end
