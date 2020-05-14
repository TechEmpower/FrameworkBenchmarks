FROM ubuntu:18.04 as builder

RUN apt-get update -yqq
RUN apt-get install -yqq wget make automake libtool file gcc-8 g++-8

WORKDIR /libreactor

ENV CC=gcc-8 AR=gcc-ar-8 NM=gcc-nm-8 RANLIB=gcc-ranlib-8

RUN wget -q https://github.com/akheron/jansson/archive/v2.12.tar.gz -O jansson-2.12.tar.gz && \
    tar xfz jansson-2.12.tar.gz && \
    cd jansson-2.12 && \
    autoreconf -fi && \
    ./configure && \
    make install

RUN wget -q https://github.com/fredrikwidlund/libdynamic/releases/download/v1.3.0/libdynamic-1.3.0.tar.gz && \
    tar xfz libdynamic-1.3.0.tar.gz && \
    cd libdynamic-1.3.0 && \
    ./configure --prefix=/usr && \
    make install

RUN wget -q https://github.com/fredrikwidlund/libclo/releases/download/v1.0.0/libclo-1.0.0.tar.gz && \
    tar xfz libclo-1.0.0.tar.gz && \
    cd libclo-1.0.0 && \
    ./configure && \
    make install

RUN wget -q https://github.com/fredrikwidlund/libreactor/releases/download/v1.0.1/libreactor-1.0.1.tar.gz && \
    tar xfz libreactor-1.0.1.tar.gz && \
    cd libreactor-1.0.1 && \
    ./configure --prefix=/usr CFLAGS="-Wall -Wextra -Wpedantic -O3" && \
    make install

COPY src/ /libreactor/src/
COPY Makefile /libreactor/Makefile

RUN make


FROM ubuntu:18.04

WORKDIR /libreactor
COPY --from=builder /libreactor .

CMD ["./libreactor"]