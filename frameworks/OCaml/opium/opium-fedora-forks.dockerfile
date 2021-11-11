FROM ocurrent/opam:fedora-32-ocaml-4.11

ENV DIR web
# https://caml.inria.fr/pub/docs/manual-ocaml/libref/Gc.html
# https://linux.die.net/man/1/ocamlrun
# https://blog.janestreet.com/memory-allocator-showdown/
ENV OCAMLRUNPARAM a=2,o=240

RUN sudo dnf install --assumeyes postgresql-devel libev-devel libffi-devel

WORKDIR /${DIR}

COPY src/opi.opam src/Makefile ./

RUN make install-ci

COPY ./src ./

RUN sudo chown -R opam: . && make build

EXPOSE 8080

ENTRYPOINT _build/default/bin/main_forks.exe
