FROM ubuntu:16.04

RUN apt-get update

# Install some common development tools
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

ENV TFVER=1.19.0

RUN apt install -yqq g++ gcc libjemalloc-dev qt5-qmake qt5-default qtbase5-dev \
    qtbase5-dev-tools libqt5sql5 libqt5sql5-mysql libqt5sql5-psql libqt5qml5 libqt5xml5 \
    qtdeclarative5-dev libqt5quick5 libqt5quickparticles5 libqt5gui5 libqt5printsupport5 \
    libqt5widgets5 libqt5opengl5-dev libqt5quicktest5

RUN wget -q https://github.com/treefrogframework/treefrog-framework/archive/v${TFVER}.tar.gz
RUN tar xf v${TFVER}.tar.gz
RUN cd treefrog-framework-${TFVER} && \
    ./configure && \
    cd src && \
    make -j4 && \
    make install && \
    cd ../tools && \
    make -j4 && \
    make install

COPY ./ ./

RUN sed -i 's|DriverType=.*|DriverType=QMYSQL|g' config/database.ini
RUN sed -i 's|MultiProcessingModule=.*|MultiProcessingModule=hybrid|g' config/application.ini

# 1. Generate Makefile
RUN qmake -r CONFIG+=release

# 2. Compile applicaton
RUN make

# 3. Start TreeFrog
CMD treefrog /
