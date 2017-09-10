#!/bin/bash

fw_installed crystal && return 0

# install crystal

VERSION="0.23.1"

SAVE_AS=crystal-$VERSION-3-linux-x86_64.tar.gz
URL=https://github.com/crystal-lang/crystal/releases/download/$VERSION/crystal-$VERSION-3-linux-x86_64.tar.gz

fw_get -o $SAVE_AS $URL

fw_untar ${SAVE_AS}

# install shards

SVERSION="0.7.1"
SAVE_AS=shards-${SVERSION}_linux_x86_64
URL=https://github.com/crystal-lang/shards/releases/download/v${SVERSION}/shards-${SVERSION}_linux_x86_64.gz

fw_get -o ${SAVE_AS}.gz $URL

gunzip ${SAVE_AS}.gz
chmod 755 ${SAVE_AS}

mv ${SAVE_AS} crystal-${VERSION}-3/bin/shards

echo -e "export PATH=${IROOT}/crystal-${VERSION}-3/bin/:\$PATH" > $IROOT/crystal.installed

source $IROOT/crystal.installed
