project_name = opi

opam_file = $(project_name).opam

.PHONY: install build run gen-opam

base:
	opam init --disable-sandboxing --auto-setup
	opam install dune

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
	opam exec -- dune build --profile release bin/main.exe

run:
	./_build/default/bin/main.exe
