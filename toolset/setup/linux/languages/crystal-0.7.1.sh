#!/bin/bash

echo "installing crystal...."

REDCODE=$(fw_exists ${IROOT}/crystal.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

SAVE_AS=crystal-0.7.1-1-linux-x86_64.tar.gz
URL=https://github.com/manastech/crystal/releases/download/0.7.1/crystal-0.7.1-1-linux-x86_64.tar.gz

# Default filename is too long, causing problems
# Use -O to specify
fw_get -O $SAVE_AS $URL

fw_untar crystal-0.7.1-1-linux-x86_64.tar.gz

touch ${IROOT}/crystal-0.7.1.installed

echo "export PATH=$IROOT/crystal-0.7.1-1/bin/:$PATH" >> ${IROOT}/crystal-0.7.1.installed
