FROM buildpack-deps:focal

RUN apt-get update -yqq
RUN apt-get install -yqq clang libboost-context-dev libboost-dev wget
RUN apt-get install -yqq libmariadb-dev

COPY ./ ./

CMD ./compile_and_start_clang.sh TFB_MYSQL
