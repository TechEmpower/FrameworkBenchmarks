FROM buildpack-deps:focal

RUN apt-get update -yqq
RUN apt-get install -yqq clang libboost-context-dev libboost-dev wget
RUN apt-get install -yqq bison flex

COPY ./ ./

RUN ./compile_libpq.sh
ENV LD_LIBRARY_PATH=/usr/lib

EXPOSE 8080

CMD ./compile.sh TFB_PGSQL 89cde5b0ef8242d697d428a8a7d429d7543450a4 
# 667f439040821d981923492e59749b87005268e4
