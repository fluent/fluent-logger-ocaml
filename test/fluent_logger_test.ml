open OUnit

module Q = Queue

type func = Write of string | Close

module Mock_sender :
  sig
    type t
    val create : int option Q.t -> t
    val write : t -> string -> int -> int -> int option
    val close : t -> unit
    val func_list : t -> func Q.t
  end =
struct
  type t = func Q.t * int option Q.t

  let create results = (Q.create (), results)

  let write sender buf start len =
      Q.push (Write (String.sub buf start len)) (fst sender);
      Q.pop (snd sender)

  let close sender = Q.push Close (fst sender)

  let func_list sender = fst sender
end

module Mock_logger = Fluent_logger.Make(Mock_sender)
module ML = Mock_logger
module MS = Mock_sender

let times n f init =
  let rec _times i a =
    if i >= n then a
    else _times (i + 1) (f i a) in
  _times 0 init

let of_string s = `FixRaw (ExtString.String.explode s)

let gen_packed_buf ?(time=Int64.of_float (Unix.time ())) tag record =
  let packed = Msgpack.Serialize.serialize_string
                 (`FixArray [of_string tag; `Uint32 time; record]) in
  (time, packed)

let test_logger_post () =
  let tag = "staging" in
  let record = `FixMap [
    (of_string "name", of_string "foobar");
    (of_string "age", `Uint8 81);
    (of_string "pi", `Float 3.14)
  ] in
  let (time, packed) = gen_packed_buf tag record in
  let results = Q.create () in
  Q.push (Some (String.length packed)) results;
  Q.push None results;
  let sender = MS.create results in
  let logger = ML.create_with_sender(sender) in
  assert_equal true (ML.post_with_time logger tag record time);
  ML.release logger;
  assert_equal (Write packed) (Q.pop (MS.func_list sender));
  assert_equal Close (Q.pop (MS.func_list sender));
  assert_raises Q.Empty (fun () -> (Q.pop (MS.func_list sender)))

let test_logger_post_many_times () =
  let tags = ["development"; "staging"; "production"; "dr"] in
  let tags_len = List.length tags in
  let gen_params_from_idx i:(string * Int64.t * Msgpack.Serialize.t * string) =
    let tag = List.nth tags (i mod tags_len) in
    let record = `FixMap [
      (of_string "name", of_string ("foobar" ^ (string_of_int (i mod 17))));
      (of_string "age", `Uint8 (i mod 100));
      (of_string "pi", `Float 3.14)
    ] in
    let time = Int64.of_int (1000000000 + i) in
    let (_, packed) = gen_packed_buf ~time:time tag record in
    (tag, time, record, packed)
  in
  let loop_max = 10000 in
  let params_list = times loop_max (fun i a -> (gen_params_from_idx i)::a) [] in
  let results = Q.create () in
  List.iter (fun (_, _, _, packed) ->
    Q.push (Some (String.length packed)) results;
  ) params_list;
  Q.push None results;
  let sender = MS.create results in
  let logger = ML.create_with_sender(sender) in
  ignore (
    times loop_max (fun i _ ->
      let (tag, time, record, packed) = List.nth params_list i in
      assert_equal true (ML.post_with_time logger tag record time);
      assert_equal (Write packed) (Q.pop (MS.func_list sender))
    ) ()
  );
  ML.release logger;
  assert_equal Close (Q.pop (MS.func_list sender));
  assert_raises Q.Empty (fun () -> (Q.pop (MS.func_list sender)))

let suite =
  "Fluent_logger tests" >:::
    ["test_logger_post" >::            test_logger_post;
     "test_logger_post_many_times" >:: test_logger_post_many_times;
    ]

let () =
  Printexc.record_backtrace true;
  ignore (run_test_tt_main suite)

