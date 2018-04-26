FROM haskell:8.2.1

RUN apt update -yqq && apt install -yqq xz-utils make
RUN apt install -yqq libpq-dev

ADD ./ /spock
WORKDIR spock

RUN stack --allow-different-user build --install-ghc

CMD stack --allow-different-user exec spock-exe -- +RTS -A32m -N$(nproc)
