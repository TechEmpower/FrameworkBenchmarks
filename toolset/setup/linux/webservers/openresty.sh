#!/bin/bash

fw_depends lua

RETCODE=$(fw_exists ${IROOT}/openresty.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/openresty.installed
  return 0; }

OPENRESTY_VERSION="1.7.10.1"
OPENRESTY=$IROOT/openresty
OPENRESTY_HOME=$OPENRESTY-$OPENRESTY_VERSION

fw_get -O http://openresty.org/download/ngx_openresty-$OPENRESTY_VERSION.tar.gz
fw_untar ngx_openresty-$OPENRESTY_VERSION.tar.gz

cd ngx_openresty-$OPENRESTY_VERSION
./configure --with-luajit-xcflags=-DLUAJIT_NUMMODE=2 --with-http_postgres_module --prefix=$OPENRESTY_HOME -j4
make -j4
make install

echo "export OPENRESTY_HOME=${OPENRESTY_HOME}" > $IROOT/openresty.installed
echo -e "export PATH=\$OPENRESTY_HOME/nginx/sbin:\$PATH" >> $IROOT/openresty.installed

source $IROOT/openresty.installed
