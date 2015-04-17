#!/bin/bash

fw_depends nim

NIMBLE_VERSION="v0.6"
NIMBLE=$NIM_HOME/nimble
RETCODE=$(fw_exists ${IROOT}/nimble.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/nimble.installed
  return 0; }

cd $NIM_HOME
# nim's package manager
git clone git://github.com/nim-lang/nimble.git
cd nimble
git checkout $NIMBLE_VERSION
../bin/nim c src/nimble
mv src/nimble ../bin/

cd $IROOT
echo "" > $IROOT/nimble.installed

source $IROOT/nimble.installed
