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

WORKDIR /installs

ENV VERSION=0.2.3
ENV CPPSP_HOME=/installs/cppsp_$VERSION

RUN wget -q http://downloads.sourceforge.net/project/cpollcppsp/CPPSP%200.2%20%28testing%29/cppsp_$VERSION.tar.xz
RUN tar xf cppsp_$VERSION.tar.xz

RUN mv cppsp_rel$VERSION/ $CPPSP_HOME

RUN sed -i 's|CXX := .*|CXX := g++-4.8|g' $CPPSP_HOME/makefile
RUN sed -i 's|-Wall|-w|g' $CPPSP_HOME/makefile

RUN apt install -yqq postgresql-server-dev-9.5
ENV CPLUS_INCLUDE_PATH=/usr/include/postgresql:/usr/include/postgresql/9.5/server:${CPLUS_INCLUDE_PATH}

ADD ./ /cpoll_cppsp
WORKDIR /cpoll_cppsp

RUN make clean && make

WORKDIR $CPPSP_HOME

CMD ./run_application /cpoll_cppsp/www -g g++-4.8 -m /forcedynamic.cppsm
