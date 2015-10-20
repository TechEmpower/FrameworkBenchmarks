#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/dub.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/dub.installed
  return 0; }

mkdir dub
cd dub
fw_get -O http://code.dlang.org/files/dub-0.9.23-linux-x86_64.tar.gz
fw_untar dub-0.9.23-linux-x86_64.tar.gz

echo -e "export PATH=${IROOT}/dub:\$PATH" > $IROOT/dub.installed

source $IROOT/dub.installed
