#!/bin/bash

NGINX_HOME=$IROOT/nginx
RETCODE=$(fw_exists ${NGINX_HOME}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $NGINX_HOME.installed
  return 0; }

fw_get -O http://nginx.org/download/nginx-1.4.1.tar.gz
fw_untar nginx-1.4.1.tar.gz
cd nginx-1.4.1

# There is no --quiet flag that I could find...
echo "Configuring nginx..."
./configure --prefix=$NGINX_HOME > /dev/null

echo "Compiling and installing nginx..."
make --quiet
make --quiet install

echo "export NGINX_HOME=${NGINX_HOME}" > $NGINX_HOME.installed
echo -e "export PATH=${IROOT}/nginx/sbin:\$PATH" >> $NGINX_HOME.installed

source $NGINX_HOME.installed
