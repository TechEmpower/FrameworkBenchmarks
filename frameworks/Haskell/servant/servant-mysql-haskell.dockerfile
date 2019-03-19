FROM haskell:8.6.3

WORKDIR /servant

COPY ./mysql-haskell/stack.yaml .
COPY ./mysql-haskell/servant-mysql-haskell.cabal .
RUN stack setup
RUN stack install --dependencies-only

ADD ./mysql-haskell/ .
RUN stack build --pedantic

CMD stack exec servant-mysql-haskell -- tfb-database +RTS -A32m -N$(nproc)
