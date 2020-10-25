BINARY = tfb

.PHONY: install build run gen-opam

install:
	opam install --yes --deps-only .

gen-opam:
	dune build @install

build:
	opam exec -- dune build --profile release $(BINARY).exe

run:
	./_build/default/bin/$(BINARY).exe
