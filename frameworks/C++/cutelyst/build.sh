#!/bin/bash

export ASQL_VER=0.46.0
export CUTELEE_VER=6.0.0
export CUTELYST_VER=3.1.0

apt update -qq && \
    apt install -yqq --no-install-recommends \
    cmake \
    git \
    pkg-config \
    qtbase5-dev \
    libqt5sql5-mysql \
    libqt5sql5-psql \
    qtdeclarative5-dev \
    postgresql-server-dev-all

wget -q https://github.com/cutelyst/cutelee/releases/download/v${CUTELEE_VER}/cutelee_${CUTELEE_VER}_amd64.deb && \
    apt install -yqq ./cutelee_${CUTELEE_VER}_amd64.deb

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
