#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/crystal.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/crystal.installed
  return 0; }

VERSION="0.7.1"

SAVE_AS=crystal-$VERSION-1-linux-x86_64.tar.gz
URL=https://github.com/manastech/crystal/releases/download/$VERSION/crystal-$VERSION-1-linux-x86_64.tar.gz

fw_get -o $SAVE_AS $URL

fw_untar crystal-$VERSION-1-linux-x86_64.tar.gz

echo -e "export PATH=${IROOT}/crystal-${VERSION}-1/bin/:\$PATH" > $IROOT/crystal.installed

source $IROOT/crystal.installed
