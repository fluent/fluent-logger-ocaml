let () =
  Printexc.record_backtrace true;
  let logger = Fluent_logger.create () in
  let open Fluent_logger.Inet in
  print_endline (string_of_bool (
    post logger "production" (
      `FixMap [
        (of_string "name", of_string "foobar");
        (of_string "age", uint8_of_int 81);
        (of_string "pi", of_float 3.14)
      ]
    )
  ));
  release logger
