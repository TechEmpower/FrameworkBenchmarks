FROM haskell:8.2.1

RUN apt update -yqq && apt install -yqq xz-utils make
RUN apt install -yqq libmysqlclient-dev pkg-config libpcre3 libpcre3-dev

COPY ./bench /snap
WORKDIR /snap

RUN stack upgrade
RUN stack --allow-different-user build --install-ghc

CMD stack --allow-different-user exec snap-bench -- +RTS -A4M -N -qg2 -I0 -G2
