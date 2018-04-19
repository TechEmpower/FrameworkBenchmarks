FROM haskell:7.10.3

RUN apt update -yqq && apt install -yqq xz-utils make
RUN apt install -yqq libpq-dev libmysqlclient-dev pkg-config libpcre3 libpcre3-dev

COPY ./yesod-mysql-mongo ./

RUN stack build -j$(nproc) --skip-ghc-check --no-terminal

CMD stack --allow-different-user exec yesod-mysql-mongo -- $(nproc) tfb-database +RTS -A32m -N$(nproc)
