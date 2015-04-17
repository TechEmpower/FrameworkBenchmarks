#!/bin/bash

JESTER=$IROOT/
RETCODE=$(fw_exists ${JESTER}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $JESTER.installed
  return 0; }

fw_depends nim

git clone https://github.com/dom96/jester.git
cd jester
git fetch origin
git checkout da9e3a73ecac51494430dce2a8387e5f0e32f968
nimble update
nimble install

echo "" > $JESTER.installed

source $JESTER.installed
