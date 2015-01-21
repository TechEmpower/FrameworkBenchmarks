#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/openresty-1.7.7.1.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_depends nginx lua

fw_get http://openresty.org/download/ngx_openresty-1.7.7.1.tar.gz
fw_untar ngx_openresty-1.7.7.1.tar.gz

cd ngx_openresty-1.7.7.1
./configure --with-luajit-xcflags=-DLUAJIT_NUMMODE=2 --with-http_postgres_module --prefix=${IROOT}/openresty-1.7.7.1 -j4
make -j4
make install

touch ${IROOT}/openresty-1.7.7.1.installed
