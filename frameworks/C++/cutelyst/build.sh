#!/bin/bash

export CUTELYST_VER=2.4.1

apt update -qq && \
    apt install -yqq --no-install-recommends \
    cmake \
    pkg-config \
    clearsilver-dev \
    libgrantlee5-dev \
    libjemalloc-dev \
    libqt5sql5-mysql \
    libqt5sql5-psql \
    uwsgi \
    uuid-dev \
    libcap-dev \
    libssl-dev \
    libzmq3-dev \
    libpcre3-dev \
    zlib1g-dev \
    nginx

wget -q https://github.com/cutelyst/cutelyst/archive/v$CUTELYST_VER.tar.gz -O cutelyst-$CUTELYST_VER.tar.gz && \
    tar zxf cutelyst-$CUTELYST_VER.tar.gz && \
    cd cutelyst-$CUTELYST_VER && mkdir build && cd build && \
    cmake .. \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX=/usr \
    -DPLUGIN_UWSGI=on \
    -DPLUGIN_VIEW_GRANTLEE=on \
    -DUSE_JEMALLOC=on && \
    make && make install

cd ${TROOT} && \
    mkdir -p build && \
    cd build && \
    cmake ${TROOT} \
    -DCMAKE_BUILD_TYPE=Release && \
    make
