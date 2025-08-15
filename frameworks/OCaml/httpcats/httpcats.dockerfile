# -*- mode: dockerfile -*-

# https://github.com/dinosaure/techempower-ocaml-image
# Use pre-built image with all dependencies for faster test times
FROM dinosaure/techempower-ocaml-image:5.3.0

# https://caml.inria.fr/pub/docs/manual-ocaml/libref/Gc.html
# https://linux.die.net/man/1/ocamlrun
# https://blog.janestreet.com/memory-allocator-showdown/
ENV OCAMLRUNPARAM a=2,o=240

COPY . /app

WORKDIR /app

RUN \
  eval $(opam env) && \
  dune build --release ./server.exe

EXPOSE 8080

CMD _build/default/server.exe
