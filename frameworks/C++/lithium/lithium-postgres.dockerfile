FROM buildpack-deps:focal

RUN apt-get update -yqq
RUN apt-get install -yqq clang bison flex libboost-dev postgresql-server-dev-all libpq-dev wget libboost-context-dev

COPY ./ ./

RUN ./compile_libpq.sh
ENV LD_LIBRARY_PATH=/usr/lib

CMD ./compile_and_start_clang.sh TFB_PGSQL
