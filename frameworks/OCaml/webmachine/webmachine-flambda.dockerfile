FROM ocurrent/opam:debian-10-ocaml-4.11-flambda

ENV DIR webmachine
# https://blog.packagecloud.io/eng/2017/02/21/set-environment-variable-save-thousands-of-system-calls/
ENV TZ  :/etc/localtime

# https://caml.inria.fr/pub/docs/manual-ocaml/libref/Gc.html
# https://linux.die.net/man/1/ocamlrun
# https://blog.janestreet.com/memory-allocator-showdown/
ENV OCAMLRUNPARAM a=2,o=240

RUN sudo apt-get install -y libpq-dev libev-dev pkg-config m4

WORKDIR /${DIR}

COPY src/tfb.opam src/Makefile /${DIR}/
COPY src/lib.opam src/Makefile /${DIR}/

RUN make install

COPY ./src /${DIR}

RUN sudo chown -R opam: . && make build

EXPOSE 8080

ENTRYPOINT _build/default/src/bin/tfb.exe
