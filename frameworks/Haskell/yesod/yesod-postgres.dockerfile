FROM haskell:7.10.3

RUN apt update -yqq && apt install -yqq xz-utils make libpq-dev libmysqlclient-dev

COPY ./yesod-postgres ./

RUN stack build -j$(nproc) --skip-ghc-check --no-terminal

CMD stack --allow-different-user exec yesod-postgres -- $(nproc) tfb-database +RTS -A32m -N$(nproc)
