#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/stack.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/stack.installed
  return 0; }

fw_get -o $IROOT/stack.tar.gz https://www.stackage.org/stack/linux-x86_64
tar xzf $IROOT/stack.tar.gz
mv $IROOT/stack-1.0.2-linux-x86_64/stack $IROOT
sudo apt-get -y install perl make automake gcc libgmp3-dev

touch $IROOT/stack.installed

source $IROOT/stack.installed
