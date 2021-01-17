FROM ocurrent/opam:alpine-3.12-ocaml-4.11

# https://caml.inria.fr/pub/docs/manual-ocaml/libref/Gc.html
# https://linux.die.net/man/1/ocamlrun
# https://blog.janestreet.com/memory-allocator-showdown/
ENV OCAMLRUNPARAM a=2,o=240

RUN sudo apk update && sudo apk add openssl-dev && \
    opam depext dune conf-libev httpaf httpaf-lwt-unix lwt yojson conf-postgresql conf-libffi

COPY src/morph-tfb.opam src/dune-project src/morph-tfb.opam.template ./

RUN opam install --yes --deps-only .

COPY ./src/bin ./bin
COPY ./src/server_io ./server_io
COPY ./src/server_io_single ./server_io_single
COPY ./src/server_io_nproc ./server_io_nproc

ENV SERVER_IO=NPROC

RUN sudo chown -R opam ./bin && sudo chown -R opam ./server_*
RUN opam exec -- dune build --profile release bin/tfb.exe

EXPOSE 8080

ENTRYPOINT _build/default/bin/tfb.exe
