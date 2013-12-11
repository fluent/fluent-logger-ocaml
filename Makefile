NAME     = fluent-logger
BASE_FILENAME = fluent_logger

VERSION  = 1.0.0

OCAMLC   = ocamlfind ocamlc
OCAMLOPT = ocamlfind ocamlopt
OCAMLDEP = ocamldep

OBJECTS  = 
XOBJECTS = 

SOURCES = $(BASE_FILENAME).mli $(BASE_FILENAME).ml

ARCHIVE  = $(BASE_FILENAME).cma
XARCHIVE = $(BASE_FILENAME).cmxa

REQUIRES = unix,extlib,msgpack

.PHONY: all opt
all: $(ARCHIVE)
opt: $(XARCHIVE)

$(ARCHIVE):
	$(OCAMLC) -a -o $(ARCHIVE) -package "$(REQUIRES)" -linkpkg $(SOURCES)

$(XARCHIVE):
	$(OCAMLOPT) -a -o $(XARCHIVE) -package "$(REQUIRES)" $(SOURCES)

.SUFFIXES: .cmo .cmi .cmx .ml .mli
.ml.cmo:
	$(OCAMLC) -package "$(REQUIRES)" -c $<

.mli.cmi:
	$(OCAMLC) -package "$(REQUIRES)" -c $<

.ml.cmx:
	$(OCAMLOPT) -package "$(REQUIRES)" -c $<

depend: *.ml *.mli
	$(OCAMLDEP) *.ml *.mli >depend

include depend

.PHONY: install uninstall
install: all
	ocamlfind install -patch-version $(VERSION) $(NAME) META $(ARCHIVE) $(BASE_FILENAME).cmi -optional $(XARCHIVE) $(BASE_FILENAME).cmx 

uninstall:
	ocamlfind remove $(NAME)

.PHONY: clean
clean:
	rm -rf *.cmi *.cmo *.cmx *.cma *.cmxa *.a *.o depend

