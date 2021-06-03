FROM haskell:8.6.3

RUN apt-get update -yqq && apt-get install -yqq xz-utils make netbase
RUN apt-get install -yqq default-libmysqlclient-dev pkg-config libpcre3 libpcre3-dev

COPY ./bench /snap
WORKDIR /snap

RUN stack upgrade
RUN stack --allow-different-user build --install-ghc

EXPOSE 8000

CMD stack --allow-different-user exec snap-bench -- +RTS -A4M -N -qg2 -I0 -G2
