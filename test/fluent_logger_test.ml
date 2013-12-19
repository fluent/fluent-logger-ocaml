type func = Write of string | Close

module Mock_sender :
  sig
    type t
    val create : int option Queue.t -> t
    val write : t -> string -> int -> int -> int option
    val close : t -> unit
  end =
struct
  type t = func Queue.t * int option Queue.t

  let create results = (Queue.create (), results)

  let write sender buf start len =
      Queue.push (Write (String.sub buf start len)) (fst sender);
      Queue.pop (snd sender)

  let close sender = Queue.push Close (fst sender)
end

module Mock_logger = Fluent_logger.Make(Mock_sender)

let () =
  Printexc.record_backtrace true;
  let results = Queue.create () in
  Queue.push (Some 42) results;
  Queue.push None results;
  let sender = Mock_sender.create results in
  let open Mock_logger in
  let logger = create_with_sender(sender) in
  ignore (
      post logger "staging" (
        `FixMap [
          (of_string "name", of_string "foobar");
          (of_string "age", uint8_of_int 81);
          (of_string "pi", of_float 3.14)
        ]));
  release logger

