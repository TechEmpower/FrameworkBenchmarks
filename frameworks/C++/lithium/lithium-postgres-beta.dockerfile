FROM buildpack-deps:focal

RUN apt-get update -yqq
RUN apt-get install -yqq clang libboost-context-dev libboost-dev wget
RUN apt-get install -yqq bison flex

COPY ./ ./

RUN ./compile_libpq.sh
ENV LD_LIBRARY_PATH=/usr/lib

EXPOSE 8080

CMD ./compile.sh TFB_PGSQL a046b3345098157849d9e2ab49a475aaabf4a90f
