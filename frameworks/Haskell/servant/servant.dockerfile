FROM haskell:8.6.3

RUN apt update -yqq && apt install -yqq xz-utils make
RUN apt install -yqq libpq-dev

WORKDIR /servant

COPY ./hasql/stack.yaml .
COPY ./hasql/servant-bench.cabal .
RUN stack setup
RUN stack install --dependencies-only

ADD ./hasql/ .
RUN stack build --pedantic

CMD stack --allow-different-user exec servant-exe -- tfb-database +RTS -A32m -N$(nproc)
