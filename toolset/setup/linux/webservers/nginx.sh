#!/bin/bash

fw_installed nginx && return 0

VERSION="1.12.0"
NGINX_HOME=$IROOT/nginx

fw_get -O http://nginx.org/download/nginx-${VERSION}.tar.gz
fw_untar nginx-${VERSION}.tar.gz
cd nginx-${VERSION}

# There is no --quiet flag that I could find...
echo "Configuring nginx..."
./configure --prefix=$NGINX_HOME > /dev/null

echo "Compiling and installing nginx..."
make --quiet
make --quiet install

echo "export NGINX_HOME=${NGINX_HOME}" > $IROOT/nginx.installed
echo -e "export PATH=\$NGINX_HOME/sbin:\$PATH" >> $IROOT/nginx.installed

source $IROOT/nginx.installed
