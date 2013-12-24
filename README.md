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
      let module L = Fluent_logger in
      let logger = (* Fluent_logger.t *)
        (* INET domain socket *)
        L.create ()
        (* UNIX domain socket *)
        (* L.create_for_unix path *) 
      in
      L.post logger "production" (
        `FixMap [
          (L.of_string "id",   L.uint8_of_int i);
          (L.of_string "name", L.of_string "foobar");
          (L.of_string "age",  L.uint8_of_int 81);
          (L.of_string "pi",   L.of_float 3.14)
        ]
      );
      L.release logger

compile:

    ocamlfind c -o example -package fluent-logger -linkpkg example.ml

