#!/bin/bash

fw_installed cppcms && return 0
fw_depends cmake libpcre3-dev zlib1g-dev libgcrypt11-dev libicu-dev

#http://cppcms.com/wikipp/en/page/cppcms_1x_build

#note '-rc1' in the url
VERSION=1.1.1
BACKNAME=cppcms
CPPCMS_HOME=$IROOT/$BACKNAME-$VERSION
CPPCMSROOT=${CPPCMS_HOME}-install


fw_get -o $BACKNAME-$VERSION.tar.bz2 https://download.sourceforge.net/project/cppcms/$BACKNAME/$VERSION-rc1/$BACKNAME-$VERSION.tar.bz2
fw_untar $BACKNAME-$VERSION.tar.bz2

cd $BACKNAME-$VERSION
rm -rf build
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=${CPPCMSROOT} ..

make
#make test
make install
#make clean

echo "export CPPCMS_HOME=${CPPCMSROOT}" > $IROOT/cppcms.installed
source $IROOT/cppcms.installed
