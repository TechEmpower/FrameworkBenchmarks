#!/bin/bash

CUTELYST_VER=0.13.3
RETCODE=$(fw_exists ${IROOT}/cutelyst.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/cutelyst.installed
  return 0; }

QT_VERSION_MM=56
QT_VERSION_FULL=561-trusty

sudo apt-add-repository --yes ppa:george-edison55/cmake-3.x
sudo apt-add-repository --yes ppa:beineri/opt-qt$QT_VERSION_FULL
sudo apt-get update -qq
sudo apt-get install -qqy cmake
sudo apt-get install -qqy uwsgi uuid-dev libcap-dev libzmq3-dev
sudo apt-get install -qqy clearsilver-dev 
sudo apt-get install -qqy qt${QT_VERSION_MM}base qt${QT_VERSION_MM}script qt${QT_VERSION_MM}tools
export CMAKE_PREFIX_PATH=/opt/qt${QT_VERSION_MM};

fw_get -O https://github.com/cutelyst/cutelyst/archive/r$CUTELYST_VER.tar.gz
fw_untar r$CUTELYST_VER.tar.gz

cd cutelyst-r$CUTELYST_VER
mkdir build && cd build

cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=$IROOT

make -j $MAX_THREADS && sudo make install

echo "" > $IROOT/cutelyst.installed

source $IROOT/cutelyst.installed
