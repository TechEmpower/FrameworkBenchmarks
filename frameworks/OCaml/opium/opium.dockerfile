FROM ocaml/opam2:alpine

USER root

RUN apk add --no-cache make m4 postgresql-dev libev-dev libffi-dev linux-headers

WORKDIR /web

RUN	opam init --disable-sandboxing --auto-setup --compiler 4.11.1\
      && opam install dune\
      && opam clean -a -c -s --logs

COPY src/opi.opam src/Makefile /web/

RUN make install-ci

COPY ./src /web

RUN eval $(opam env) ; dune build --profile release /web/bin/main.exe

CMD /web/_build/default/bin/main.exe
