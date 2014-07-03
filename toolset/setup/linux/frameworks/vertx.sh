#!/bin/bash
. ../toolset/setup/linux/bash_functions.sh

fw_exists vert.x-2.1.1
[ $? -ne 0 ] || { echo "Vert.x is installed!"; return 0; }

fw_get http://dl.bintray.com/vertx/downloads/vert.x-2.1.1.tar.gz?direct=true -O vert.x-2.1.1.tar.gz
fw_untar vert.x-2.1.1.tar.gz
