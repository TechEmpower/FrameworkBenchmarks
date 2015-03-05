#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/nginx.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://nginx.org/download/nginx-1.6.2.tar.gz
fw_untar nginx-1.6.2.tar.gz
cd nginx-1.6.2

# There is no --quiet flag that I could find...
echo "Configuring nginx..."
./configure --prefix=$IROOT/nginx > /dev/null

echo "Compiling and installing nginx..."
make --quiet
make --quiet install

touch ${IROOT}/nginx.installed