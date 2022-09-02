FROM ubuntu:20.04

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
	 apt-get install -yqq gcc-10 g++-10

RUN locale-gen en_US.UTF-8

ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

ENV CC=gcc-10
ENV CXX=g++-10
ENV AR=gcc-ar-10
ENV RANLIB=gcc-ranlib-10

ENV IROOT=/install
ENV DROGON_ROOT=$IROOT/drogon
ENV MIMALLOC_ROOT=$IROOT/mimalloc
ENV PG_ROOT=$IROOT/postgres-batch_mode_ubuntu
ENV TEST_PATH=/drogon_benchmark/build

WORKDIR $IROOT

RUN sudo sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list'
RUN wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | sudo apt-key add -
RUN sudo apt -y update
RUN sudo apt -y install postgresql-server-dev-all

RUN git clone https://github.com/an-tao/drogon

WORKDIR $DROGON_ROOT

RUN git checkout ebf87d69d7bb45dfa478ba364ef9374d9be25092
RUN git submodule update --init
RUN mkdir build

WORKDIR $DROGON_ROOT/build

RUN cmake -DCMAKE_BUILD_TYPE=release -DCMAKE_CXX_FLAGS=-flto ..
RUN make && make install

WORKDIR $IROOT

RUN git clone https://github.com/microsoft/mimalloc

WORKDIR $MIMALLOC_ROOT
RUN git checkout v1.6.7 -b v1.6.7
RUN mkdir -p out/release
WORKDIR $MIMALLOC_ROOT/out/release
RUN cmake -DCMAKE_BUILD_TYPE=release -DCMAKE_CXX_FLAGS=-flto ../..
RUN make && make install

WORKDIR $TEST_PATH

RUN cmake -DCMAKE_BUILD_TYPE=release -DCMAKE_CXX_FLAGS=-flto ..
RUN make

EXPOSE 8080

CMD ./drogon_benchmark config-core.json
