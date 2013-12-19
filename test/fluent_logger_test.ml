open OUnit

type func = Write of string | Close

module Mock_sender :
  sig
    type t
    val create : int option Queue.t -> t
    val write : t -> string -> int -> int -> int option
    val close : t -> unit
    val func_list : t -> func Queue.t
  end =
struct
  type t = func Queue.t * int option Queue.t

  let create results = (Queue.create (), results)

  let write sender buf start len =
      Queue.push (Write (String.sub buf start len)) (fst sender);
      Queue.pop (snd sender)

  let close sender = Queue.push Close (fst sender)

  let func_list sender = fst sender
end

module Mock_logger = Fluent_logger.Make(Mock_sender)

let of_string s = `FixRaw (ExtString.String.explode s)

let test_logger_post () =
  let tag = "staging" in
  let time = Int64.of_float (Unix.time ()) in
  let record = `FixMap [
    (of_string "name", of_string "foobar");
    (of_string "age", `Uint8 81);
    (of_string "pi", `Float 3.14)
  ] in
  let packed = Msgpack.Serialize.serialize_string
                 (`FixArray [of_string tag; `Uint32 time; record]) in
  let results = Queue.create () in
  Queue.push (Some (String.length packed)) results;
  Queue.push None results;
  let sender = Mock_sender.create results in
  let open Mock_logger in
  let logger = create_with_sender(sender) in
  assert_equal true (post_with_time logger tag record time);
  release logger;
  assert_equal (Write packed) (Queue.pop (Mock_sender.func_list sender));
  assert_equal Close (Queue.pop (Mock_sender.func_list sender));
  assert_raises Queue.Empty (fun () ->
    (Queue.pop (Mock_sender.func_list sender))
  )

let suite =
  "Fluent_logger tests" >::: ["test_logger_post" >:: test_logger_post]

let () =
  Printexc.record_backtrace true;
  ignore (run_test_tt_main suite)

