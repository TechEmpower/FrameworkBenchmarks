FROM ubuntu:22.04
MAINTAINER shaovie@gmail.com

RUN apt-get update -yqq
RUN apt-get install -yqq g++-11 gcc-11 make git

RUN   update-alternatives --quiet --remove-all gcc \
    ; update-alternatives --quiet --remove-all g++ \
    ; update-alternatives --quiet --remove-all cc \
    ; update-alternatives --quiet --remove-all cpp \
    ; update-alternatives --quiet --install /usr/bin/gcc gcc /usr/bin/gcc-11 20 \
    ; update-alternatives --quiet --install /usr/bin/cc cc /usr/bin/gcc-11 20 \
    ; update-alternatives --quiet --install /usr/bin/g++ g++ /usr/bin/g++-11 20 \
    ; update-alternatives --quiet --install /usr/bin/cpp cpp /usr/bin/g++-11 20 \
    ; update-alternatives --quiet --config gcc \
    ; update-alternatives --quiet --config cc \
    ; update-alternatives --quiet --config g++ \
    ; update-alternatives --quiet --config cpp


WORKDIR /reactor-bench

RUN git clone https://github.com/shaovie/reactor.git


RUN cd reactor/ && make clean all
COPY ./techempower.cpp /reactor-bench/reactor

WORKDIR /reactor-bench/reactor
RUN g++ techempower.cpp -O2 -std=c++11 -lreactor -L./bin -lpthread -o app

ENV LD_LIBRARY_PATH=./bin:$LD_LIBRARY_PATH

EXPOSE 8080

RUN ulimit -n 100000
CMD ./app
