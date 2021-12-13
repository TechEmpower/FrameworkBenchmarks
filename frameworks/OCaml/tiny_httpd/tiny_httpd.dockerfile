FROM ocurrent/opam:debian-10-ocaml-4.11-flambda

ENV DIR tiny_httpd
# https://blog.packagecloud.io/eng/2017/02/21/set-environment-variable-save-thousands-of-system-calls/
ENV TZ  :/etc/localtime

# https://caml.inria.fr/pub/docs/manual-ocaml/libref/Gc.html
# https://linux.die.net/man/1/ocamlrun
# https://blog.janestreet.com/memory-allocator-showdown/
ENV OCAMLRUNPARAM a=2,o=240

WORKDIR /${DIR}

RUN opam install tiny_httpd atdgen

COPY ./src /${DIR}

RUN sudo chown -R opam: . && eval $(opam env) && dune build --profile release

EXPOSE 8080

ENTRYPOINT _build/default/src/bin/tiny.exe
