FROM haskell:8.2.1

RUN apt update -yqq
RUN apt install -yqq xz-utils make > /dev/null
RUN apt install -yqq libpq-dev > /dev/null

ADD ./ /servant
WORKDIR /servant

RUN stack --allow-different-user setup
RUN stack --allow-different-user build

CMD stack --allow-different-user exec servant-exe -- tfb-database +RTS -A32m -N$(nproc)
