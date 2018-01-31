#!/bin/bash

fw_installed crystal && return 0

# install crystal and shards (shards is embedded in crystal release)

VERSION="0.24.1"

SAVE_AS=crystal-$VERSION-2-linux-x86_64.tar.gz
URL=https://github.com/crystal-lang/crystal/releases/download/v$VERSION/crystal-$VERSION-2-linux-x86_64.tar.gz

fw_get -o $SAVE_AS $URL

fw_untar ${SAVE_AS}

echo -e "export PATH=${IROOT}/crystal-${VERSION}/bin/:\$PATH" > $IROOT/crystal.installed

source $IROOT/crystal.installed
