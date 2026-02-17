FROM haskell:9.10.3-slim-bookworm

RUN apt-get update -yqq && apt-get install -yqq xz-utils make curl ca-certificates
RUN install -d /usr/share/postgresql-common/pgdg
RUN curl -o /usr/share/postgresql-common/pgdg/apt.postgresql.org.asc --fail https://www.postgresql.org/media/keys/ACCC4CF8.asc
RUN . /etc/os-release
RUN sh -c "echo 'deb [signed-by=/usr/share/postgresql-common/pgdg/apt.postgresql.org.asc] https://apt.postgresql.org/pub/repos/apt bookworm-pgdg main' > /etc/apt/sources.list.d/pgdg.list"
RUN apt-get update && apt-get install -yqq libpq-dev

WORKDIR /app

COPY stack.yaml ./
COPY ./shared/tfb-types/tfb-types.cabal ./shared/tfb-types/
COPY ./shared/tfb-hasql/tfb-hasql.cabal ./shared/tfb-hasql/
COPY ./shared/tfb-mysql-haskell/tfb-mysql-haskell.cabal ./shared/tfb-mysql-haskell/
COPY ./shared/tfb-postgres-simple/tfb-postgres-simple.cabal ./shared/tfb-postgres-simple/
COPY ./warp-shared/warp-shared.cabal ./warp-shared/
RUN stack setup
RUN stack install --dependencies-only

ADD ./shared ./shared
ADD ./warp-shared ./warp-shared
RUN stack build --pedantic --copy-bins
RUN ln -s ~/.local/bin/warp-postgres-simple ~/.local/bin/warp

ARG TFB_TEST_NAME
ENV TFB_TEST_NAME=${TFB_TEST_NAME}

EXPOSE 7041

CMD stack exec $TFB_TEST_NAME -- tfb-database +RTS -A32m -N$(nproc)
