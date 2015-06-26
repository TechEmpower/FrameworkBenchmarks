#!/bin/bash

fw_depends nim nimble zeromq mongrel2

RETCODE=$(fw_exists ${IROOT}/nawak.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/nawak.installed
  return 0; }

git clone https://github.com/idlewan/nawak.git
cd nawak
git fetch
git checkout b34b0b5077541ae9671957452a70e2578894d3a8
nimble update
nimble install

echo "export NAWAK_HOME=${IROOT}/nawak" > $IROOT/nawak.installed

source $IROOT/nawak.installed
