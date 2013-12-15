REQUIRES=msgpack
SRCS=inet_sender.mli inet_sender.ml fluent_logger.mli fluent_logger.ml

.PHONY: all opt clean

all: fluent_logger.cma

opt: fluent_logger.cmxa

clean:
	rm -rf *.cmi *.cmo *.cmx *.cma *.cmxa *.a *.o

fluent_logger.cma: $(SRCS)
	$(OCAMLC) -a -o $@ -package "$(REQUIRES)" $(SRCS)

fluent_logger.cmxa: $(SRCS)
	$(OCAMLOPT) -a -o $@ -package "$(REQUIRES)" $(SRCS)

depend: *.ml *.mli
	$(OCAMLDEP) *.ml *.mli >depend

include depend

.SUFFIXES: .cmo .cmi .cmx .ml .mli

.ml.cmo:
	$(OCAMLC) -package "$(REQUIRES)" -c $<

.mli.cmi:
	$(OCAMLC) -package "$(REQUIRES)" -c $<

.ml.cmx:
	$(OCAMLOPT) -package "$(REQUIRES)" -c $<

