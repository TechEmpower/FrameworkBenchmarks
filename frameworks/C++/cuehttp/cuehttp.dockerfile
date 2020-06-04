FROM ubuntu:18.04

RUN apt-get update -yqq
RUN apt-get install -yqq g++-7 cmake git uuid-dev libboost-all-dev

ENV CUEHTTP=/cuehttp

WORKDIR /
RUN git clone https://github.com/xcyl/cuehttp.git

WORKDIR /cuehttp
RUN git checkout 55a2667caddf8516b3262b8d51e19a0e1d0aab0a

WORKDIR /cuehttp/examples/plaintext
RUN mkdir build && cd build && cmake .. && make -j8
EXPOSE 10001
CMD ./build/plaintext
