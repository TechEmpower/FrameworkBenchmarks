#!/bin/bash

VERSION="1.7.10.1"
OPENRESTY=$IROOT/openresty-$VERSION
RETCODE=$(fw_exists ${OPENRESTY}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $OPENRESTY.installed
  return 0; }

fw_depends nginx lua

fw_get -O http://openresty.org/download/ngx_openresty-${VERSION}.tar.gz
fw_untar ngx_openresty-${VERSION}.tar.gz

cd ngx_openresty-${VERSION}
./configure --with-luajit-xcflags=-DLUAJIT_NUMMODE=2 --with-http_postgres_module --prefix=${IROOT}/openresty-${VERSION} -j4
make -j4
make install

echo "export OPENRESTY_HOME=${OPENRESTY}" > $OPENRESTY.installed
echo -e "export PATH=${OPENRESTY}/nginx/sbin:\$PATH" >> $OPENRESTY.installed

source $OPENRESTY.installed
