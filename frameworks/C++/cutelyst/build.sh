#!/bin/bash

export ASQL_VER=0.74.0
export CUTELEE_VER=6.1.0
export CUTELYST_VER=3.7.0

apt update -qq && \
    apt install -yqq --no-install-recommends \
    cmake \
    git \
    pkg-config \
    qt6-base-dev \
    libqt6sql6-mysql \
    libqt6sql6-psql \
    libegl1-mesa-dev \
    postgresql-server-dev-all

wget -q https://github.com/cutelyst/cutelee/releases/download/v${CUTELEE_VER}/cutelee6-qt6_${CUTELEE_VER}_amd64.deb && \
    apt install -yqq ./cutelee6-qt6_${CUTELEE_VER}_amd64.deb

wget -q https://github.com/cutelyst/asql/releases/download/v${ASQL_VER}/libasql0-qt6_${ASQL_VER}_amd64.deb && \
    apt install -yqq ./libasql0-qt6_${ASQL_VER}_amd64.deb

wget -q https://github.com/cutelyst/cutelyst/releases/download/v${CUTELYST_VER}/cutelyst3-qt6_${CUTELYST_VER}_amd64.deb && \
    apt install -yqq ./cutelyst3-qt6_${CUTELYST_VER}_amd64.deb

cd ${TROOT} && \
    mkdir -p build && \
    cd build && \
    cmake ${TROOT} \
    -DCMAKE_BUILD_TYPE=Release && \
    make
