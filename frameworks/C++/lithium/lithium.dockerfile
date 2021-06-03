FROM buildpack-deps:focal

RUN apt-get update -yqq
RUN apt-get install -yqq clang libboost-context-dev libboost-dev wget
RUN apt-get install -yqq libmariadb-dev

COPY ./ ./

EXPOSE 8080

CMD ./compile.sh TFB_MYSQL
