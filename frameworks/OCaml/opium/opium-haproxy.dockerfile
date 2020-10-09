FROM ocurrent/opam:alpine-3.12-ocaml-4.11

USER root

# RUN apk add --no-cache make m4 postgresql-dev libev-dev libffi-dev linux-headers

WORKDIR /web

# RUN	opam init --disable-sandboxing --auto-setup\
#       && opam install dune\
#       && opam clean -a -c -s --logs

COPY ./src/opi.opam ./src/Makefile /web/

# RUN make install-ci

COPY ./src /web

RUN echo '#a#' && ls\
    && echo '#b#' && ls /web\
    && echo '#c#' && ls /web/bin\
    && echo '#d#' && ls /web/lib\
    && echo '#e#' && find . -type f -name 'main.ml'\

# RUN eval $(opam env) ; dune build --profile release ./bin/main.exe
