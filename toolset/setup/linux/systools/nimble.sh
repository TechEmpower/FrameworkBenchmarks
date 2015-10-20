#!/bin/bash

fw_depends nim

RETCODE=$(fw_exists ${IROOT}/nimble.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/nimble.installed
  return 0; }
  
NIMBLE_VERSION="0.6.2"

cd $NIM_HOME
# nim's package manager
fw_get -O https://github.com/nim-lang/nimble/archive/v$NIMBLE_VERSION.tar.gz
fw_untar v$NIMBLE_VERSION.tar.gz
mv nimble-$NIMBLE_VERSION nimble
cd nimble
../bin/nim c src/nimble
mv src/nimble ../bin/

cd $IROOT
echo "" > $IROOT/nimble.installed

source $IROOT/nimble.installed
