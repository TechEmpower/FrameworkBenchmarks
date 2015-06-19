#!/bin/bash

VERSION=1.4.2
GOROOT=$IROOT/go
RETCODE=$(fw_exists ${GOROOT}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $GOROOT.installed
  return 0; }

fw_get -O https://storage.googleapis.com/golang/go$VERSION.linux-amd64.tar.gz
fw_untar go$VERSION.linux-amd64.tar.gz

echo "export GOROOT=${IROOT}/go" > $GOROOT.installed
echo -e "export PATH=${GOROOT}/bin:\$PATH" >> $GOROOT.installed

source $GOROOT.installed
