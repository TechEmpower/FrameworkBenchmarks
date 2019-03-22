FROM ubuntu:18.04

COPY ./ ./

RUN  apt update -yqq && \
     apt-get install -yqq software-properties-common && \
	 apt install -yqq sudo curl wget cmake locales git \
     openssl libssl-dev \
     libjsoncpp-dev \
     uuid-dev \
     zlib1g-dev \
	 postgresql-server-dev-all && \
     add-apt-repository ppa:ubuntu-toolchain-r/test -y && \
	 apt update -yqq && \
	 apt install -yqq gcc-8 g++-8

RUN locale-gen en_US.UTF-8

ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

ENV CC=gcc-8
ENV CXX=g++-8
ENV AR=gcc-ar-8
ENV RANLIB=gcc-ranlib-8

ENV IROOT=/install
ENV DROGON_ROOT=$IROOT/drogon
ENV TEST_PATH=/drogon_benchmark/build
WORKDIR $IROOT

RUN git clone https://github.com/an-tao/drogon

WORKDIR $DROGON_ROOT

RUN git checkout 1e1bcbf781d9e2cf3a9d4110ef6fc6875c41a1bd

RUN ./build.sh

WORKDIR $TEST_PATH

RUN cmake ..
RUN make
CMD ./drogon_benchmark config.json

