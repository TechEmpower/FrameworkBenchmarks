#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/libevent.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/libevent.installed
  return 0; }

sudo apt-get install -qqy libevent-dev

fw_get -O https://ccodearchive.net/tarballs/json.tar.bz2
tar xf $IROOT/json.tar.bz2

touch $IROOT/libevent.installed
