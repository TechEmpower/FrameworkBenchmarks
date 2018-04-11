FROM ubuntu:16.04

RUN apt-get update

RUN apt-get install -y software-properties-common build-essential curl locales wget unzip git \
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

RUN apt install -yqq libgcrypt11-dev cmake python nginx

WORKDIR /installs

#http://cppcms.com/wikipp/en/page/cppcms_1x_build
#note '-rc1' in the url
ENV CPPCMS_VERSION=1.1.1
ENV BACKNAME=cppcms
ENV CPPCMS_HOME=/installs/$BACKNAME-$CPPCMS_VERSION
ENV CPPCMSROOT=${CPPCMS_HOME}-install

RUN wget -q https://download.sourceforge.net/project/cppcms/$BACKNAME/$CPPCMS_VERSION-rc1/$BACKNAME-$CPPCMS_VERSION.tar.bz2
RUN tar xf $BACKNAME-$CPPCMS_VERSION.tar.bz2

RUN cd $BACKNAME-$CPPCMS_VERSION && \
    mkdir build && \
    cd build && \
    cmake -DCMAKE_INSTALL_PREFIX=${CPPCMSROOT} .. && \
    make && make install

ENV CPPCMS_HOME=${CPPCMSROOT}

ENV CPPDB_VERSION=0.3.1
ENV BACKNAME=cppdb
ENV CPPDB_HOME=/installs/$BACKNAME-$CPPDB_VERSION
ENV CPPDBROOT=${CPPDB_HOME}-install

RUN wget -q https://download.sourceforge.net/project/cppcms/$BACKNAME/$CPPDB_VERSION/$BACKNAME-$CPPDB_VERSION.tar.bz2
RUN tar xf $BACKNAME-$CPPDB_VERSION.tar.bz2

RUN cd $BACKNAME-$CPPDB_VERSION && \
    mkdir build && cd build && \
    cmake -DCMAKE_INSTALL_PREFIX=${CPPDBROOT} .. && \
    make && make install

ENV CPPDB_HOME=${CPPDBROOT}

ENV LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${CPPCMS_HOME}/lib:${CPPDB_HOME}/lib

WORKDIR /cppcms
COPY src src
COPY config-nginx-mysql.json config-nginx-mysql.json
COPY Makefile Makefile
COPY nginx.conf nginx.conf

RUN make

CMD nginx -c /cppcms/nginx.conf && ./mycppcms -c config-nginx-mysql.json
