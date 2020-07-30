FROM buildpack-deps:focal

RUN apt-get update -yqq
RUN apt-get install -yqq g++-9 libboost-dev postgresql-server-dev-all libpq-dev wget libboost-context-dev

COPY ./ ./

CMD ./compile_and_start_gcc.sh TFB_PGSQL 3
