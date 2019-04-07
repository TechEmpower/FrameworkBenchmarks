FROM haskell:8.6.3

RUN apt update -yqq && apt install -yqq xz-utils make
RUN apt install -yqq libpq-dev

WORKDIR /app

COPY ./stack.yaml .
COPY ./hasql/servant-hasql.cabal ./hasql/
COPY ./mysql-haskell/servant-mysql-haskell.cabal ./mysql-haskell/
RUN stack setup
RUN stack install --dependencies-only

ADD ./hasql/ ./hasql/
ADD ./mysql-haskell/ ./mysql-haskell/
RUN stack build --pedantic --copy-bins
RUN ln -s ~/.local/bin/servant-hasql ~/.local/bin/servant

ARG TFB_TEST_NAME
ENV TFB_TEST_NAME=${TFB_TEST_NAME}
CMD stack exec $TFB_TEST_NAME -- tfb-database +RTS -A32m -N$(nproc)
