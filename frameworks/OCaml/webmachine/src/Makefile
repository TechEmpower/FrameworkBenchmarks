BINARY = tfb
PROFILE ?= release

.PHONY: install build run gen-opam

install:
	opam install --yes --deps-only .

gen-opam:
	dune build @install

build:
	opam exec -- dune build --profile $(PROFILE)

run:
	./_build/default/src/bin/$(BINARY).exe
