type dst_info = INET of string * int | UNIX of string
type t
val create:  dst_info -> int -> t
val close: t -> unit
val write: t -> string -> int -> int -> int option
