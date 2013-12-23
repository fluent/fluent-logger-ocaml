fluent-logger-ocaml
===================

A structured logger for Fluentd (OCaml)


Features
------------

* Inet/Unix domain sockets are available
* Buffering data on memory if failed to send
* Persistent connection
* Exponential backoff for reconnection


Build
------------

    $ make clean
    $ make
    $ make install

Build
------------

    $ make test

Usage
------------

example.ml:

    let () =
      let logger = (* Fluent_logger.Stream.t *)
        (* INET domain socket *)
        Fluent_logger.create ()
        (* UNIX domain socket *)
        (* Fluent_logger.create_for_unix path *) 
      in
      let open Fluent_logger.Stream in
      post logger "production" (
        `FixMap [
          (of_string "id", uint8_of_int i);
          (of_string "name", of_string "foobar");
          (of_string "age", uint8_of_int 81);
          (of_string "pi", of_float 3.14)
        ]
      );
      release logger

compile:

    ocamlfind c -o example -package fluent-logger -linkpkg example.ml

