FROM ocurrent/opam:fedora-32-ocaml-4.11

ENV DIR webmachine

# https://caml.inria.fr/pub/docs/manual-ocaml/libref/Gc.html
# https://linux.die.net/man/1/ocamlrun
# https://blog.janestreet.com/memory-allocator-showdown/
ENV OCAMLRUNPARAM a=2,o=240

RUN sudo dnf install --assumeyes diffutils postgresql-devel libev-devel

WORKDIR /${DIR}

COPY src/webmachine-tfb.opam src/Makefile /${DIR}/

RUN make install

COPY ./src /${DIR}

RUN sudo chown -R opam: . && make build

CMD _build/default/tfb.exe
