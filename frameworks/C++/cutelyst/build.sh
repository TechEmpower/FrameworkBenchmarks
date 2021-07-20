#!/bin/bash

export ASQL_VER=0.43.0
export CUTELEE_VER=5.3.0
export CUTELYST_VER=2.14.0

apt update -qq && \
    apt install -yqq --no-install-recommends \
    cmake \
    pkg-config \
    qtbase5-dev \
    libqt5sql5-mysql \
    libqt5sql5-psql \
    qtdeclarative5-dev \
    postgresql-server-dev-all

wget -q https://github.com/cutelyst/cutelee/releases/download/v${CUTELEE_VER}/cutelee5_${CUTELEE_VER}_amd64.deb && \
    apt install -yqq ./cutelee5_${CUTELEE_VER}_amd64.deb

wget -q https://github.com/cutelyst/asql/releases/download/v${ASQL_VER}/libasql_${ASQL_VER}_amd64.deb && \
    apt install -yqq ./libasql_${ASQL_VER}_amd64.deb

wget -q https://github.com/cutelyst/cutelyst/releases/download/v${CUTELYST_VER}/cutelyst_${CUTELYST_VER}_amd64.deb && \
    apt install -yqq ./cutelyst_${CUTELYST_VER}_amd64.deb

cd ${TROOT} && \
    mkdir -p build && \
    cd build && \
    cmake ${TROOT} \
    -DCMAKE_BUILD_TYPE=Release && \
    make
