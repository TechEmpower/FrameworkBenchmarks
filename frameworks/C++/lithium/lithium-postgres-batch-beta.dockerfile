
FROM buildpack-deps:focal

RUN apt-get update -yqq
RUN apt-get install -yqq clang libboost-context-dev libboost-dev wget
RUN apt-get install -yqq bison flex

COPY ./ ./

RUN ./compile_libpq.sh
ENV LD_LIBRARY_PATH=/usr/lib

CMD ./compile-batch.sh TFB_PGSQL 78ccb291a94679bf567ece2428d2763d80fc0029
