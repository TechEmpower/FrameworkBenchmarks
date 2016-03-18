#!/bin/bash

fw_depends mono

RETCODE=$(fw_exists ${IROOT}/xsp.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/xsp.installed
  return 0; }

sudo apt-get install -y mono-fastcgi-server

echo "" > $IROOT/xsp.installed
