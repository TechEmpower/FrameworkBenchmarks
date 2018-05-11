FROM haskell:8.2.1

RUN apt update -yqq
RUN apt install -yqq xz-utils make > /dev/null
RUN apt install -yqq libpq-dev > /dev/null

ADD ./ /spock
WORKDIR spock

RUN stack --allow-different-user build --install-ghc

CMD stack --allow-different-user exec spock-exe -- +RTS -A32m -N$(nproc)
