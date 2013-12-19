PKG_NAME  = fluent-logger

export OCAMLC   = ocamlfind ocamlc -g
export OCAMLOPT = ocamlfind ocamlopt -g
export OCAMLDEP = ocamldep

.PHONY: all opt install uninstall test example clean

all:
	make -C lib all

opt:
	make -C lib opt

install: all opt
	ocamlfind install $(PKG_NAME) META \
        lib/fluent_logger.cma lib/fluent_logger.cmi \
        -optional \
        lib/fluent_logger.cmxa lib/fluent_logger.cmx lib/fluent_logger.a

uninstall:
	ocamlfind remove $(PKG_NAME)

example:
	make -C example all

test:
	make -C test all

clean:
	make -C example clean
	make -C test clean
	make -C lib clean

