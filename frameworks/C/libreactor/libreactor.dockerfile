FROM ubuntu:20.04 as builder

RUN apt-get update -yqq
RUN apt-get install -yqq wget git make automake libtool file gcc-10 g++-10

WORKDIR /build

ENV CC=gcc-10 AR=gcc-ar-10 NM=gcc-nm-10 RANLIB=gcc-ranlib-10

RUN wget -q https://github.com/fredrikwidlund/libdynamic/releases/download/v2.2.0/libdynamic-2.2.0.tar.gz && \
    tar xfz libdynamic-2.2.0.tar.gz && \
    cd libdynamic-2.2.0 && \
    ./configure && \
    make install

# Remove the unused "#include <dynamic.h>" directive since it causes a build error: "unknown type name 'pthread_t'".
# Specify configure prefix so libclo gets installed in /usr/lib like libdynamic and libreactor.
# Add march=native. Both libdynamic and libreactor are already using it.
RUN wget -q https://github.com/fredrikwidlund/libclo/releases/download/v1.0.0/libclo-1.0.0.tar.gz && \
    tar xfz libclo-1.0.0.tar.gz && \
    cd libclo-1.0.0 && \
    sed -i '/#include <dynamic.h>/d' ./src/clo.c && \
    ./configure --prefix=/usr CFLAGS="-march=native" && \
    make install

RUN git clone https://github.com/fredrikwidlund/libreactor --single-branch --branch release-2.0 libreactor-2 && \
    cd libreactor-2 && \
    git checkout 63fa717a8047 && \
    ./autogen.sh && \
    ./configure && \
    make install

COPY src/ /build/src/
COPY Makefile /build/Makefile

RUN make libreactor


FROM ubuntu:20.04

WORKDIR /app
COPY --from=builder /build/libreactor .

RUN groupadd -r libreactor && useradd --no-log-init -r -g libreactor libreactor
USER libreactor

EXPOSE 8080

CMD ["./libreactor"]