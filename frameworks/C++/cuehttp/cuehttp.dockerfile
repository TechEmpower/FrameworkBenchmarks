FROM ubuntu:18.04

RUN apt-get update -yqq
RUN apt-get install -yqq g++-7 cmake git libboost-all-dev

ENV CUEHTTP=/cuehttp

WORKDIR /
RUN git clone https://github.com/xcyl/cuehttp.git

WORKDIR /cuehttp

RUN git checkout a7f5a4c935e22d110b70c5928c8ea2ce4dcbeeb5

WORKDIR /cuehttp/examples/plaintext
RUN mkdir build && cd build && cmake .. && make -j8
EXPOSE 8080
CMD ./build/plaintext
