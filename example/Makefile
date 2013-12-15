SRCS=example.ml
REQUIRES=fluent-logger

.PHONY: all run clean

all: example

example: $(SRCS)
	$(OCAMLC) -o $@ -package "$(REQUIRES)" -linkpkg $(SRCS)

run:
	make example
	./example

clean:
	rm -rf example *.cmi *.cmo
