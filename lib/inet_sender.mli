type t
val create_for_inet :
  ?conn_timeout:int -> ?host:string -> ?port:int -> unit -> t
val create : ?conn_timeout:int -> ?host:string -> ?port:int -> unit -> t
val write : t -> string -> int -> int -> int option
val close : t -> unit
