FROM ubuntu:18.04

COPY ./ ./

RUN  apt-get update -yqq && \
     apt-get install -yqq software-properties-common && \
	 apt-get install -yqq sudo curl wget cmake locales git \
     openssl libssl-dev \
     libjsoncpp-dev \
     uuid-dev libreadline-dev libbison-dev flex \
     zlib1g-dev && \
     add-apt-repository ppa:ubuntu-toolchain-r/test -y && \
	 apt-get update -yqq && \
	 apt-get install -yqq gcc-8 g++-8

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
ENV MIMALLOC_ROOT=$IROOT/mimalloc
ENV PG_ROOT=$IROOT/postgres-batch_mode_ubuntu
ENV TEST_PATH=/drogon_benchmark/build

WORKDIR $IROOT

RUN wget https://github.com/an-tao/postgres/archive/batch_mode_ubuntu.tar.gz
RUN tar -xvzf batch_mode_ubuntu.tar.gz
WORKDIR $PG_ROOT

RUN ./configure --prefix=/usr CFLAGS='-O2 -pipe'
RUN make && make install

WORKDIR $IROOT

RUN git clone https://github.com/an-tao/drogon

WORKDIR $DROGON_ROOT

RUN git checkout 668533fbbd20eb9c493841b0e1067097aadf0342
RUN git submodule update --init
RUN mkdir build

WORKDIR $DROGON_ROOT/build

RUN cmake -DCMAKE_BUILD_TYPE=release ..
RUN make && make install

WORKDIR $IROOT

RUN git clone https://github.com/microsoft/mimalloc

WORKDIR $MIMALLOC_ROOT
RUN git checkout v1.6.2 -b v1.6.2
RUN mkdir -p out/release
WORKDIR $MIMALLOC_ROOT/out/release
RUN cmake ../..
RUN make && make install

WORKDIR $TEST_PATH

RUN cmake -DCMAKE_BUILD_TYPE=release ..
RUN make
CMD ./drogon_benchmark config-core.json
