FROM buildpack-deps:focal

RUN apt-get update -yqq
RUN apt-get install -yqq libboost-dev libmariadb-dev wget  libboost-context-dev clang

COPY ./ ./

CMD ./compile_and_start.sh TFB_MYSQL