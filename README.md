fluent-logger-ocaml
===================

A structured logger for Fluentd (OCaml)

Build
------------

    $ make
    $ make opt # optional
    $ make install
       or
    $ sudo make install # if needed


Usage
------------

example.ml:

    let () =
      let open Fluent_logger in
      let logger = create () in
      post logger "production" (
        `FixMap [
          (of_string "name", of_string "foobar");
          (of_string "age", uint8_of_int 81);
          (of_string "pi", of_float 3.14)
        ]
      );
      release logger

compile:

    ocamlfind c -o example -package fluent-logger -linkpkg example.ml

