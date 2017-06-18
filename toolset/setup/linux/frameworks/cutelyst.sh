#!/bin/bash

fw_installed cutelyst && return 0

CUTELYST_VER=1.7.0
QT_VERSION_MM=59
QT_VERSION_FULL=59-trusty
CROOT=${IROOT}/cutelyst

sudo apt-add-repository --yes ppa:george-edison55/cmake-3.x
sudo apt-add-repository --yes ppa:beineri/opt-qt$QT_VERSION_FULL
sudo apt-get update -qq

sudo apt-get install -qqy \
cmake \
uwsgi \
uuid-dev \
libcap-dev \
libzmq3-dev \
clearsilver-dev \
libjemalloc-dev \
qt${QT_VERSION_MM}base \
qt${QT_VERSION_MM}script \
qt${QT_VERSION_MM}tools

export CMAKE_PREFIX_PATH=/opt/qt${QT_VERSION_MM};

mkdir -p ${CROOT} || true
cd ${CROOT}

fw_get -O https://github.com/cutelyst/cutelyst/archive/v$CUTELYST_VER.tar.gz
fw_untar v$CUTELYST_VER.tar.gz

cd cutelyst-$CUTELYST_VER
mkdir build && cd build

cmake .. \
-DCMAKE_BUILD_TYPE=Release \
-DCMAKE_INSTALL_PREFIX=$CROOT \
-DUWSGI_PLUGINS_DIR=${CROOT}/lib/uwsgi/plugins \
-DUSE_JEMALLOC=on

make -j $CPU_COUNT && make install

echo "QT_VERSION_MM=${QT_VERSION_MM}" > $IROOT/cutelyst.installed

