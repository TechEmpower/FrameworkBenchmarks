FROM haskell:8.6.3

RUN apt update -yqq && apt install -yqq xz-utils make
RUN apt install -yqq libpq-dev

WORKDIR /app

COPY ./stack.yaml .
COPY ./warp-postgres-wire/warp-postgres-wire.cabal ./warp-postgres-wire/
COPY ./warp-hasql/warp-hasql.cabal ./warp-hasql/
RUN stack setup
RUN stack install --dependencies-only

ADD ./warp-postgres-wire/ ./warp-postgres-wire
ADD ./warp-hasql/ ./warp-hasql
RUN stack build --pedantic --copy-bins
RUN ln -s ~/.local/bin/warp-hasql ~/.local/bin/warp

ARG TFB_TEST_NAME
ENV TFB_TEST_NAME=${TFB_TEST_NAME}
CMD stack exec $TFB_TEST_NAME -- tfb-database +RTS -A32m -N$(nproc)
