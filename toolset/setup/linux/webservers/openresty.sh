#!/bin/bash

fw_exists /usr/local/openresty/nginx/sbin/nginx
[ $? -ne 0 ] || { return 0; }

fw_depends nginx
fw_get http://openresty.org/download/ngx_openresty-1.5.8.1.tar.gz
fw_untar ngx_openresty-1.5.8.1.tar.gz
cd ngx_openresty-1.5.8.1
./configure --with-luajit --with-http_postgres_module
make
sudo make install