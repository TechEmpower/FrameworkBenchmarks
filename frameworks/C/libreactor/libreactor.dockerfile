FROM ubuntu:16.04

RUN apt update -yqq
RUN apt install -yqq software-properties-common python-software-properties wget make

RUN add-apt-repository ppa:ubuntu-toolchain-r/test -y && \
    apt update -yqq && \
    apt install -yqq gcc-6 g++-6

ADD ./ /libreactor
WORKDIR /libreactor

RUN wget -q https://github.com/fredrikwidlund/libdynamic/releases/download/v1.1.0/libdynamic-1.1.0.tar.gz && \
    tar xfz libdynamic-1.1.0.tar.gz && \
    cd libdynamic-1.1.0 && \
    ./configure CC=gcc-6 AR=gcc-ar-6 NM=gcc-nm-6 RANLIB=gcc-ranlib-6 && \
    make && make install

RUN wget -q https://github.com/fredrikwidlund/libreactor/releases/download/v1.0.0/libreactor-1.0.0.tar.gz && \
    tar xfz libreactor-1.0.0.tar.gz && \
    cd libreactor-1.0.0 && \
    ./configure CC=gcc-6 AR=gcc-ar-6 NM=gcc-nm-6 RANLIB=gcc-ranlib-6 && \
    make && make install

RUN wget -q https://github.com/fredrikwidlund/libclo/releases/download/v0.1.0/libclo-0.1.0.tar.gz && \
    tar xfz libclo-0.1.0.tar.gz && \
    cd libclo-0.1.0 && \
    ./configure CC=gcc-6 AR=gcc-ar-6 NM=gcc-nm-6 RANLIB=gcc-ranlib-6 && \
    make && make install

RUN make clean && make

CMD ["./libreactor"]
