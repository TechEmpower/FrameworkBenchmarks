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
RUN apt update -y
RUN apt install -qqy g++-4.8
RUN update-alternatives --install /usr/bin/g++ g++ /usr/bin/g++-4.8 50

ENV POCO_VERSION 1.6.1
ENV POCO_HOME /poco

WORKDIR ${POCO_HOME}
RUN curl -sL http://pocoproject.org/releases/poco-${POCO_VERSION}/poco-${POCO_VERSION}-all.tar.gz | tar xz --strip-components=1

RUN ./configure --no-tests --no-samples
RUN make --quiet PageCompiler-libexec XML-libexec JSON-libexec

ENV LD_LIBRARY_PATH ${POCO_HOME}/lib/Linux/x86_64

COPY benchmark.cpp benchmark.cpp

RUN g++-4.8 \
    -O3 \
    -DNDEBUG \
    -std=c++0x \
    -o \
    poco \
    benchmark.cpp \
    -I${POCO_HOME}/Foundation/include \
    -I${POCO_HOME}/Util/include \
    -I${POCO_HOME}/Net/include \
    -L${POCO_HOME}/lib/Linux/x86_64 \
    -lPocoNet \
    -lPocoUtil \
    -lPocoFoundation \
    -lPocoXML \
    -lPocoJSON

CMD ./poco 8080 $(nproc)
