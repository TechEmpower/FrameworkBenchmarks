FROM buildpack-deps:focal

RUN apt-get update -yqq
RUN apt-get install -yqq clang libboost-dev postgresql-server-dev-all libpq-dev wget libboost-context-dev

COPY ./ ./

CMD ./compile_and_start_clang.sh TFB_PGSQL
