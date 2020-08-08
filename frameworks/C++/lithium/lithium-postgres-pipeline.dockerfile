FROM buildpack-deps:focal

RUN apt-get update -yqq
RUN apt-get install -yqq clang libboost-dev bison flex wget libboost-context-dev

COPY ./ ./

RUN ./compile_libpq.sh batchmode
ENV LD_LIBRARY_PATH=/usr/lib

CMD ./compile_clang-pipeline.sh TFB_PGSQL 0
