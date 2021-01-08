
FROM buildpack-deps:focal

RUN apt-get update -yqq
RUN apt-get install -yqq clang libboost-context-dev libboost-dev wget
RUN apt-get install -yqq bison flex

COPY ./ ./

RUN ./compile_libpq.sh batchmode
ENV LD_LIBRARY_PATH=/usr/lib

CMD ./compile-batch.sh TFB_PGSQL  16b3f43a2b292e5349b7b528fa9410b3239c15cb
