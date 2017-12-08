#!/bin/bash

fw_installed play1 && return 0

VERSION="1.5.0"
PLAY1_HOME=$IROOT/play-$VERSION

fw_get -O https://downloads.typesafe.com/play/$VERSION/play-$VERSION.zip
fw_unzip play-$VERSION.zip

echo "export PLAY1_HOME=${PLAY1_HOME}" > $IROOT/play1.installed
echo -e "export PATH=\$PLAY1_HOME:\$PATH" >> $IROOT/play1.installed

source $IROOT/play1.installed
