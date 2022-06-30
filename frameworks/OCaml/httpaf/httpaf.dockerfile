# -*- mode: dockerfile -*-

FROM alpine:3.15

# https://caml.inria.fr/pub/docs/manual-ocaml/libref/Gc.html
# https://linux.die.net/man/1/ocamlrun
# https://blog.janestreet.com/memory-allocator-showdown/
ENV OCAMLRUNPARAM a=2,o=240

RUN apk add --no-cache \
    bash\
    bubblewrap\
    coreutils\
    gcc\
    git\
    libev-dev\
    libffi-dev\
    linux-headers\
    m4\
    make\
    musl-dev\
    opam\
    postgresql-dev

RUN opam init\
    --disable-sandboxing\
    --auto-setup\
    --compiler ocaml-base-compiler.4.13.1

RUN opam install -y opam-depext
RUN \
  opam depext -y dune conf-libev httpaf httpaf-lwt-unix lwt yojson && \
  opam install -y dune conf-libev httpaf httpaf-lwt-unix lwt yojson

COPY . /app

WORKDIR /app

RUN \
  eval $(opam env) && \
  dune build --release httpaf_unix.exe

EXPOSE 8080

CMD _build/default/httpaf_unix.exe
