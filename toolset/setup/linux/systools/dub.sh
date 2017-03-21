#!/bin/bash

fw_installed dub && return 0

mkdir dub
cd dub
fw_get -O http://code.dlang.org/files/dub-1.0.0-linux-x86_64.tar.gz
fw_untar dub-1.0.0-linux-x86_64.tar.gz

echo -e "export PATH=${IROOT}/dub:\$PATH" > $IROOT/dub.installed

source $IROOT/dub.installed
