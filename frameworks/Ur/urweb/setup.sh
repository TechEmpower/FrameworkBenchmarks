#!/bin/bash

VERSION=20160213
COMPILER=${IROOT}/urweb

RETCODE=$(fw_exists ${COMPILER}.installed)
[ "$RETCODE" == 0 ] || { \
  cd $IROOT
  fw_get -O http://www.impredicative.com/ur/urweb-$VERSION.tgz
  fw_untar urweb-$VERSION.tgz
  cd urweb-$VERSION
  ./configure --prefix=$IROOT/urweb
  make
  make install

  echo "export URWEB_HOME=${COMPILER}" > $COMPILER.installed
  echo "export LD_LIBRARY_PATH=${COMPILER}/lib" >> $COMPILER.installed
  echo -e "export PATH=${COMPILER}/bin:\$PATH" >> $COMPILER.installed
  cd $TROOT
}

source $IROOT/urweb.installed

urweb -db "dbname=hello_world user=benchmarkdbuser password=benchmarkdbpass host=${DBHOST}" bench

MAX_THREADS=$((2 * $MAX_THREADS))
./bench.exe -q -k -t ${MAX_THREADS} &
