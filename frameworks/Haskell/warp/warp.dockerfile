FROM haskell:8.6.3

RUN apt update -yqq && apt install -yqq xz-utils make

WORKDIR /app

COPY ./stack.yaml .
COPY ./warp-postgres-wire/warp-postgres-wire.cabal ./warp-postgres-wire/
RUN stack setup
RUN stack install --dependencies-only

ADD ./warp-postgres-wire/ ./warp-postgres-wire
RUN stack build --pedantic

CMD stack exec warp-postgres-wire -- tfb-database +RTS -A32m -N$(nproc)
