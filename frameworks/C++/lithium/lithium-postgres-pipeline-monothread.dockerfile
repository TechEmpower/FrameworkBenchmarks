FROM buildpack-deps:focal

RUN apt-get update -yqq
RUN apt-get install -yqq clang libboost-dev bison flex wget libboost-context-dev

COPY ./ ./

CMD ./compile_and_start_clang-pipeline.sh TFB_PGSQL 1
