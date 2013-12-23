let () =
  Printexc.record_backtrace true;
  let logger = 
    if Array.length Sys.argv > 0 then
      (* use 'path' parameter for unix domain socket *)
      Fluent_logger.create_for_unix Sys.argv.(1)
    else
      (* inet domain socket *)
      Fluent_logger.create ()
  in
  let open Fluent_logger.Stream in
  let rec loop n i f = if i < n then (f i; loop n (i + 1) f) in
  loop 240 0 (fun i ->
    let result =
      post logger "production" (
        `FixMap [
          (of_string "id", uint8_of_int i);
          (of_string "name", of_string "foobar");
          (of_string "age", uint8_of_int 81);
          (of_string "pi", of_float 3.14)
        ]
      ) in
    if not result then prerr_endline "logger: failed to post";
    Unix.sleep 1
  );
  release logger
