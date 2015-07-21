#!/bin/bash

DUB=$IROOT/dub
RETCODE=$(fw_exists ${DUB}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $DUB.installed
  return 0; }

mkdir dub
cd dub
fw_get -O http://code.dlang.org/files/dub-0.9.23-linux-x86_64.tar.gz
fw_untar dub-0.9.23-linux-x86_64.tar.gz

echo -e "export PATH=${DUB}:\$PATH" > $DUB.installed

source $DUB.installed
