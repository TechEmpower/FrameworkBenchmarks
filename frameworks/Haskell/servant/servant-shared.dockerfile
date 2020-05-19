FROM haskell:8.6.3

RUN apt update -yqq && apt install -yqq xz-utils make
RUN apt install -yqq libpq-dev

WORKDIR /app

COPY stack.yaml ./
COPY ./shared/tfb-types/tfb-types.cabal ./shared/tfb-types/
COPY ./shared/tfb-hasql/tfb-hasql.cabal ./shared/tfb-hasql/
COPY ./shared/tfb-mysql-haskell/tfb-mysql-haskell.cabal ./shared/tfb-mysql-haskell/
COPY ./shared/tfb-postgres-wire/tfb-postgres-wire.cabal ./shared/tfb-postgres-wire/
COPY ./servant-shared/servant-shared.cabal ./servant-shared/
RUN stack setup
RUN stack install --dependencies-only

ADD ./shared ./shared
ADD ./servant-shared ./servant-shared
RUN stack build --pedantic --copy-bins
RUN ln -s ~/.local/bin/servant-hasql ~/.local/bin/servant

ARG TFB_TEST_NAME
ENV TFB_TEST_NAME=${TFB_TEST_NAME}
CMD stack exec $TFB_TEST_NAME -- tfb-database +RTS -A32m -N$(nproc)
