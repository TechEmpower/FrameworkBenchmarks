FROM haskell:7.10.3

RUN apt-get update -yqq && apt-get install -yqq xz-utils make netbase
RUN apt-get install -yqq libpq-dev libmysqlclient-dev pkg-config libpcre3 libpcre3-dev

COPY ./yesod-mysql-mongo ./

RUN stack build -j$(nproc) --skip-ghc-check --no-terminal

EXPOSE 8000

CMD stack --allow-different-user exec yesod-mysql-mongo -- $(nproc) tfb-database +RTS -A32m -N$(nproc)
