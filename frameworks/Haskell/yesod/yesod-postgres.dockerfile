FROM haskell:8.2.1

RUN apt update -yqq && apt install -yqq xz-utils make libpq-dev libmysqlclient-dev

COPY ./yesod-postgres ./

RUN stack --allow-different-user build --install-ghc

CMD stack --allow-different-user exec yesod-postgres -- $(nproc) tfb-database +RTS -A32m -N$(nproc)
