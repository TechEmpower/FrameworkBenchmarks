FROM haskell:8.6.3

RUN apt-get update -yqq && apt-get install -yqq xz-utils make
RUN apt-get install -yqq libpq-dev

ADD ./ /spock
WORKDIR spock

RUN stack --allow-different-user build --install-ghc

EXPOSE 3000

CMD stack --allow-different-user exec spock-exe -- +RTS -A32m -N$(nproc)
