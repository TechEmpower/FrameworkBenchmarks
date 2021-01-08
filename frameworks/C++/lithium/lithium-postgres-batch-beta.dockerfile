
FROM buildpack-deps:focal

RUN apt-get update -yqq
RUN apt-get install -yqq clang libboost-context-dev libboost-dev wget
RUN apt-get install -yqq bison flex

COPY ./ ./

RUN ./compile_libpq.sh batchmode
ENV LD_LIBRARY_PATH=/usr/lib

CMD ./compile-batch.sh TFB_PGSQL 6cb70b6e7c2748b6b0db5d203856860f5e7025f7
