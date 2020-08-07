FROM buildpack-deps:focal

RUN apt-get update -yqq
RUN apt-get install -yqq clang libboost-dev bison flex wget libboost-context-dev

COPY ./ ./

RUN ./compile_clang-pipeline.sh TFB_PGSQL 1


ENV LD_LIBRARY_PATH=/usr/lib
CMD /lithium_tbf tfb-database 8080
