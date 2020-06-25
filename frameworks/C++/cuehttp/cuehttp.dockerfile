FROM ubuntu:18.04

RUN apt-get update -yqq
RUN apt-get install -yqq g++-7 cmake git uuid-dev libboost-all-dev

ENV CUEHTTP=/cuehttp

WORKDIR /
RUN git clone https://github.com/xcyl/cuehttp.git

WORKDIR /cuehttp
RUN git checkout tfb

WORKDIR /cuehttp/examples/plaintext
RUN mkdir build && cd build && cmake .. && make -j8
EXPOSE 8080
CMD ./build/plaintext
