# -*- mode: dockerfile -*-

FROM ocurrent/opam:alpine-3.12-ocaml-4.11

RUN \
  opam depext dune conf-libev httpaf httpaf-lwt-unix lwt yojson && \
  opam install dune conf-libev httpaf httpaf-lwt-unix lwt yojson

COPY . /app

WORKDIR /app

RUN \
  sudo chown -R opam: . && \
  eval $(opam env) && \
  dune build --release httpaf_unix.exe

EXPOSE 8080

CMD _build/default/httpaf_unix.exe
