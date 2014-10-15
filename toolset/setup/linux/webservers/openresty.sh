#!/bin/bash

RETCODE=$(fw_exists openresty.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_depends nginx lua

fw_get http://openresty.org/download/ngx_openresty-1.7.4.1.tar.gz
fw_untar ngx_openresty-1.7.4.1.tar.gz

cd ngx_openresty-1.7.4.1
./configure --with-luajit-xcflags=-DLUAJIT_NUMMODE=2 --with-http_postgres_module -j4
make -j4
sudo make install

touch $IROOT/openresty.installed
