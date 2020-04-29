FROM ubuntu:18.04 as builder

RUN apt-get update -yqq
RUN apt-get install -yqq wget make automake libtool file gcc-8 g++-8

WORKDIR /libreactor

ENV CC=gcc-8 AR=gcc-ar-8 NM=gcc-nm-8 RANLIB=gcc-ranlib-8

RUN wget -q https://github.com/fredrikwidlund/libdynamic/releases/download/v1.1.0/libdynamic-1.1.0.tar.gz && \
    tar xfz libdynamic-1.1.0.tar.gz && \
    cd libdynamic-1.1.0 && \
    ./configure && \
    make && make install

RUN wget -q https://github.com/fredrikwidlund/libreactor/releases/download/v1.0.0/libreactor-1.0.0.tar.gz && \
    tar xfz libreactor-1.0.0.tar.gz && \
    cd libreactor-1.0.0 && \
    ./configure && \
    make && make install

RUN wget -q https://github.com/fredrikwidlund/libclo/releases/download/v0.1.0/libclo-0.1.0.tar.gz && \
    tar xfz libclo-0.1.0.tar.gz && \
    cd libclo-0.1.0 && \
    ./configure  && \
    make && make install

COPY src/ /libreactor/src/
COPY Makefile /libreactor/Makefile

RUN make


FROM ubuntu:18.04

WORKDIR /libreactor
COPY --from=builder /libreactor .

CMD ["./libreactor"]
