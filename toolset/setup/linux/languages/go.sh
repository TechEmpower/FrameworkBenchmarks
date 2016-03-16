#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/go.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/go.installed
  return 0; }

VERSION=1.6
GOROOT=$IROOT/go

fw_get -O https://storage.googleapis.com/golang/go$VERSION.linux-amd64.tar.gz
fw_untar go$VERSION.linux-amd64.tar.gz

echo "export GOROOT=${IROOT}/go" > $IROOT/go.installed
echo -e "export GOPATH=\$TROOT" >> $IROOT/go.installed
echo -e "export PATH=\$GOROOT/bin:\$GOPATH/bin:\$PATH" >> $IROOT/go.installed
echo "export GOGC=1000" >> $IROOT/go.installed

source $IROOT/go.installed
