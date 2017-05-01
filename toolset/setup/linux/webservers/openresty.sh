#!/bin/bash

fw_depends lua

fw_installed openresty && return 0

OPENRESTY_VERSION="1.11.2.1"
OPENRESTY=$IROOT/openresty
OPENRESTY_HOME=$OPENRESTY-$OPENRESTY_VERSION

fw_get -O http://openresty.org/download/openresty-$OPENRESTY_VERSION.tar.gz
fw_untar openresty-$OPENRESTY_VERSION.tar.gz

cd openresty-$OPENRESTY_VERSION
./configure --with-http_postgres_module --prefix=$OPENRESTY_HOME --with-luajit-xcflags="-DLUAJIT_NUMMODE=2 -O3" --with-cc-opt="-O3" -j4
make -j4 --quiet
make --quiet install

echo "export OPENRESTY_HOME=${OPENRESTY_HOME}" > $IROOT/openresty.installed
echo -e "export PATH=\$OPENRESTY_HOME/nginx/sbin:\$PATH" >> $IROOT/openresty.installed

source $IROOT/openresty.installed
