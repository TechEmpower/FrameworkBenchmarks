project_name = opi
BINARY = main

.PHONY: install build run gen-opam

install-ci: pin install clean-cache

pin:
	opam pin add --no-action --yes opium_kernel https://github.com/rgrinberg/opium.git\#a840e4653387fc80529b1c48bc3c66b682eb1426
	opam pin add --no-action --yes opium        https://github.com/rgrinberg/opium.git\#a840e4653387fc80529b1c48bc3c66b682eb1426

install:
	opam install --yes --deps-only .

clean-cache:
	opam clean -a -c -s --logs

gen-opam:
	dune build @install

build:
	opam exec -- dune build --profile release

run:
	dune exec $(project_name) -- --debug

run-forks:
	dune exec $(project_name)_forks -- --debug
