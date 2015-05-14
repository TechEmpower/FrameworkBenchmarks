#!/bin/bash

DLANG=$IROOT/dlang
RETCODE=$(fw_exists ${DLANG}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $DLANG.installed
  return 0; }

fw_get http://downloads.dlang.org/releases/2.x/2.067.1/dmd_2.067.1-0_amd64.deb
dpkg -i dmd_2.067.1-0_amd64.deb --instdir=$DLANG

echo -e "export PATH=${DLANG}:\$PATH" > $DLANG.installed

source $DLANG.installed
