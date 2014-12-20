#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/nginx.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://nginx.org/download/nginx-1.4.1.tar.gz
fw_untar nginx-1.4.1.tar.gz
cd nginx-1.4.1

# There is no --quiet flag that I could find...
echo "Configuring nginx..."
./configure --prefix=$IROOT/nginx > /dev/null

echo "Compiling and installing nginx..."
make --quiet
make --quiet install

touch ${IROOT}/nginx.installed