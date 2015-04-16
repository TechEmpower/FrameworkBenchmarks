#!/bin/bash

VERSION="2.0.6"
SIENNA=$IROOT/siena-$VERSION
RETCODE=$(fw_exists ${SIENA}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $SIENNA.installed
  return 0; }

fw_depends play1
yes | play install siena-2.0.6

echo "" > $SIENA.installed

source $SIENA.installed
