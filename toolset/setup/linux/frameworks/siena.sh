#!/bin/bash

fw_depends play1

RETCODE=$(fw_exists ${IROOT}/siena.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/siena.installed
  return 0; }

VERSION="2.0.6"
SIENNA=$IROOT/siena-$VERSION

yes | play install siena-2.0.6

echo "" > $IROOT/siena.installed

source $IROOT/siena.installed
