FROM ubuntu:20.04
MAINTAINER oleg@romanenko.ro

ENV DEBIAN_FRONTEND noninteractive

RUN apt-get -qq -y update

RUN apt-get -qq -y install --no-install-recommends git cmake libev-dev libgoogle-perftools-dev libfmt-dev make gcc-9 g++-9 libre2-dev libboost-stacktrace-dev libhttp-parser-dev libxxhash-dev

RUN   update-alternatives --quiet --remove-all gcc \
    ; update-alternatives --quiet --remove-all g++ \
    ; update-alternatives --quiet --remove-all cc \
    ; update-alternatives --quiet --remove-all cpp \
    ; update-alternatives --quiet --install /usr/bin/gcc gcc /usr/bin/gcc-9 20 \
    ; update-alternatives --quiet --install /usr/bin/cc cc /usr/bin/gcc-9 20 \
    ; update-alternatives --quiet --install /usr/bin/g++ g++ /usr/bin/g++-9 20 \
    ; update-alternatives --quiet --install /usr/bin/cpp cpp /usr/bin/g++-9 20 \
    ; update-alternatives --quiet --config gcc \
    ; update-alternatives --quiet --config cc \
    ; update-alternatives --quiet --config g++ \
    ; update-alternatives --quiet --config cpp


COPY ./libsniper_bench /libsniper_bench

WORKDIR /libsniper_bench

RUN git config --global http.sslverify false

ENV LIBSNIPER_VER v1.3.1

RUN git clone https://github.com/rtbtech/libsniper.git libs/core

RUN cd libs/core && git checkout v1.3.1

RUN mkdir build && cd /libsniper_bench/build && cmake -DCMAKE_BUILD_TYPE=Release -S .. && make --jobs=`nproc`

ARG BENCHMARK_ENV

EXPOSE 8090

CMD ./build/bin/libsniper_bench
