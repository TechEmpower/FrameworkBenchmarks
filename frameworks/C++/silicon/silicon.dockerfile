FROM ubuntu:16.04

RUN apt-get update

RUN apt-get install -qqy software-properties-common build-essential curl locales wget unzip git \
    libmysqlclient-dev libpq-dev \
    libpcre3 libpcre3-dev \
    libssl-dev libcurl4-openssl-dev \
    zlib1g-dev \
    libreadline6-dev \
    libbz2-dev \
    libxslt-dev libgdbm-dev ncurses-dev  \
    libffi-dev libtool bison libevent-dev \
    libgstreamer-plugins-base0.10-0 libgstreamer0.10-0 \
    liborc-0.4-0 libgnutls-dev \
    libjson0-dev libmcrypt-dev libicu-dev \
    re2c libnuma-dev

RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8
ENV DEBIAN_FRONTEND noninteractive

RUN add-apt-repository -s "deb http://apt.llvm.org/`lsb_release -cs`/ llvm-toolchain-`lsb_release -cs`-3.9 main"
RUN wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key| apt-key add -
RUN apt -yq update
RUN apt install -qqy clang-3.9 lldb-3.9

ENV MICROHTTPD_VERSION=0.9.39
ENV MICROHTTPD=/libmicrohttpd
ENV MICROHTTPD_HOME=$MICROHTTPD-$VERSION

RUN wget http://mirror.ibcp.fr/pub/gnu/libmicrohttpd/libmicrohttpd-$MICROHTTPD_VERSION.tar.gz
RUN tar xf libmicrohttpd-$MICROHTTPD_VERSION.tar.gz
RUN cd libmicrohttpd-$MICROHTTPD_VERSION && \
    ./configure --prefix=$MICROHTTPD_HOME && \
    make install

ENV PATH=${MICROHTTPD_HOME}/bin:${PATH}

RUN apt install -yqq libboost-dev cmake

ENV SILICON=/silicon

COPY ./ ./

RUN git clone https://github.com/matt-42/silicon.git && \
    cd silicon && \
    git checkout ecaf04887c9dbbf0f457afab1f487268f6aeffab && \
    CC=clang-3.9 CXX=clang++-3.9 ./install.sh /

RUN cd build && \
    cmake .. -DCMAKE_CXX_COMPILER=clang++-3.9 && \
    make silicon_tpc_mysql

CMD /build/silicon_tpc_mysql tfb-database 8080 $(nproc)
