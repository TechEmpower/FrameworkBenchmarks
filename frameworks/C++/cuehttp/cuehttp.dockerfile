FROM ubuntu:18.04

RUN apt-get update -yqq
RUN apt-get install -yqq g++-7 cmake git libboost-all-dev

ENV CUEHTTP=/cuehttp

WORKDIR /
RUN git clone https://github.com/xcyl/cuehttp.git

WORKDIR /cuehttp
RUN git checkout b504d22388f9566712cc366a81da5be040645ef2

WORKDIR /cuehttp/examples/plaintext
RUN mkdir build && cd build && cmake .. && make -j8
EXPOSE 8080
CMD ./build/plaintext
