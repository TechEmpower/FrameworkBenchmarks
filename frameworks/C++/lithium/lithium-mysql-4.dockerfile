FROM buildpack-deps:focal

RUN apt-get update -yqq
RUN apt-get install -yqq libboost-dev libmariadb-dev wget  libboost-context-dev g++-9

COPY ./ ./

CMD ./compile_and_start_gcc.sh TFB_MYSQL 4
