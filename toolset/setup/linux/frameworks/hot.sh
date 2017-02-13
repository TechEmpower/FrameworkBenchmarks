#!/bin/bash

fw_installed hot && return 0

VERSION="0.9.2-SNAPSHOT"
HOT_HOME=$IROOT/hot-$VERSION

fw_get -O https://github.com/dsolimando/Hot/releases/download/${VERSION}/hot-${VERSION}.tar.gz
fw_untar hot-$VERSION.tar.gz

echo "export HOT_HOME=${HOT_HOME}" > $IROOT/hot.installed
echo -e "export PATH=\$HOT_HOME:\$PATH" >> $IROOT/hot.installed

source $IROOT/hot.installed
