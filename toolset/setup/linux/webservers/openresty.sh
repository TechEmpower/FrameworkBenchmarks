#!/bin/bash

RETCODE=$(fw_exists /usr/local/openresty/nginx/sbin/nginx)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_depends nginx
fw_get http://openresty.org/download/ngx_openresty-1.5.12.1.tar.gz
fw_untar ngx_openresty-1.5.12.1.tar.gz
cd ngx_openresty-1.5.12.1
./configure --with-luajit-xcflags=-DLUAJIT_NUMMODE=2 --with-cc-opt=-O2 --with-http_postgres_module -j2
make -j2
sudo make install
