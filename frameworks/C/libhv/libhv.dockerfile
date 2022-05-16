FROM ubuntu:20.04

RUN apt-get update -yqq
RUN apt-get install -y git build-essential

WORKDIR /
RUN git clone -b tfb https://github.com/ithewei/libhv.git
WORKDIR /libhv
RUN ./configure && make tinyhttpd

EXPOSE 8080

CMD ./bin/tinyhttpd 8080
