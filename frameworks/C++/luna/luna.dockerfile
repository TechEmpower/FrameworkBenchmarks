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

RUN add-apt-repository -y ppa:ubuntu-toolchain-r/test
RUN apt update -qqy
RUN apt install -qqy g++-4.9
RUN update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.9 50

RUN apt install -qqy python-dev python-pip cmake autoconf
RUN pip install -q conan==0.28.1

WORKDIR /luna
COPY CMakeLists.txt CMakeLists.txt
COPY common.h common.h
COPY conanfile.txt conanfile.txt
COPY default.cpp default.cpp
COPY epoll.cpp epoll.cpp
COPY thread.cpp thread.cpp

RUN CC=gcc-4.9 CXX=g++-4.9 conan install --build=missing -s compiler="gcc" -s compiler.version="4.9" . >/dev/null
RUN cmake . -DCMAKE_CXX_COMPILER=g++-4.9 -DCMAKE_CC_COMPILER=gcc-4.9 >/dev/null
RUN cmake --build . >/dev/null

CMD /luna/bin/lunabench_default 8080 $((2 * $(nproc)))
