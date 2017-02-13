#!/bin/bash

VERSION=20160621
COMPILER=${IROOT}/urweb

RETCODE=$(fw_exists ${COMPILER}.installed)
[ "$RETCODE" == 0 ] || { \
  sudo apt-get --assume-yes install mlton
  cd $IROOT
  fw_get -O http://www.impredicative.com/ur/urweb-$VERSION.tgz
  fw_untar urweb-$VERSION.tgz
  cd urweb-$VERSION
  sudo ./configure --prefix=$IROOT/urweb
  sudo make
  sudo make install

  echo "export URWEB_HOME=${COMPILER}" > $COMPILER.installed
  echo "export LD_LIBRARY_PATH=${COMPILER}/lib" >> $COMPILER.installed
  echo -e "export PATH=${COMPILER}/bin:\$PATH" >> $COMPILER.installed
  cd $TROOT
}

source $IROOT/urweb.installed

