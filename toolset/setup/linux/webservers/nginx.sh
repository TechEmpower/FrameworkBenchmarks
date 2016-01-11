#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/nginx.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/nginx.installed
  return 0; }

NGINX_HOME=$IROOT/nginx

fw_get -O http://nginx.org/download/nginx-1.9.9.tar.gz
fw_untar nginx-1.9.9.tar.gz
cd nginx-1.9.9

# There is no --quiet flag that I could find...
echo "Configuring nginx..."
./configure --prefix=$NGINX_HOME > /dev/null

echo "Compiling and installing nginx..."
make --quiet
make --quiet install

echo "export NGINX_HOME=${NGINX_HOME}" > $IROOT/nginx.installed
echo -e "export PATH=\$NGINX_HOME/sbin:\$PATH" >> $IROOT/nginx.installed

source $IROOT/nginx.installed
