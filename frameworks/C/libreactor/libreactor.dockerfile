FROM ubuntu:20.04 as builder

RUN apt-get update -yqq
RUN apt-get install -yqq wget make automake libtool file gcc-9 g++-9

WORKDIR /libreactor

ENV CC=gcc-9 AR=gcc-ar-9 NM=gcc-nm-9 RANLIB=gcc-ranlib-9

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
    ./configure --prefix=/usr CFLAGS="-Wall -Wextra -Wpedantic -O3 -g" && \
    make install

COPY src/ /libreactor/src/
COPY Makefile /libreactor/Makefile

RUN make


FROM ubuntu:20.04

WORKDIR /libreactor
COPY --from=builder /libreactor .

CMD ["./libreactor"]