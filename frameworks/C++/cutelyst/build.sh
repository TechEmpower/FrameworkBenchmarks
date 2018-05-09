#!/bin/bash

export CUTELYST_VER=2.2.2

export QT_VERSION_MM=59
export QT_VERSION_FULL=594-xenial

export CMAKE_PREFIX_PATH=/opt/qt${QT_VERSION_MM}

echo ${CMAKE_PREFIX_PATH}/lib > /etc/ld.so.conf.d/qt${QT_VERSION_MM}.conf
ldconfig

apt-add-repository --yes ppa:beineri/opt-qt$QT_VERSION_FULL && \
    apt update -qq && \
    apt install -yqq \
    cmake \
    clearsilver-dev \
    libgrantlee5-dev \
    libjemalloc-dev \
    qt${QT_VERSION_MM}base \
    qt${QT_VERSION_MM}script \
    qt${QT_VERSION_MM}tools

apt install -yqq uwsgi uwsgi uuid-dev libcap-dev libzmq3-dev nginx

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
