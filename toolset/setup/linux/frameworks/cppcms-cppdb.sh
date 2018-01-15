#!/bin/bash

fw_installed cppcms-cppdb && return 0
fw_depends cppcms
#libmysqld-dev
fw_depends cmake libpq-dev

#http://cppcms.com/sql/cppdb/build.html

VERSION=0.3.1
BACKNAME=cppdb
CPPDB_HOME=$IROOT/$BACKNAME-$VERSION
CPPDBROOT=${CPPDB_HOME}-install

fw_get -o $BACKNAME-$VERSION.tar.bz2 https://download.sourceforge.net/project/cppcms/$BACKNAME/$VERSION/$BACKNAME-$VERSION.tar.bz2
fw_untar $BACKNAME-$VERSION.tar.bz2

cd $BACKNAME-$VERSION
rm -rf build
mkdir build
cd build
cmake -DCMAKE_INSTALL_PREFIX=${CPPDBROOT} ..

make
#make test
sudo make install
#make clean


echo "export CPPDB_HOME=${CPPDBROOT}" > $IROOT/cppcms-cppdb.installed
source $IROOT/cppcms-cppdb.installed
