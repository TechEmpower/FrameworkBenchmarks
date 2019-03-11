FROM haskell:8.6.3

RUN apt update -yqq && apt install -yqq xz-utils make
RUN apt install -yqq libpq-dev

ADD ./ /servant
WORKDIR /servant

RUN stack --allow-different-user setup
RUN stack --allow-different-user build --pedantic

CMD stack --allow-different-user exec servant-exe -- tfb-database +RTS -A32m -N$(nproc)
