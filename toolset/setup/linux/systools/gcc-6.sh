#!/bin/bash


RETCODE=$(fw_exists ${IROOT}/gcc-6.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/gcc-6.installed
  return 0; }

sudo add-apt-repository ppa:ubuntu-toolchain-r/test -y
sudo apt-get -yq update
sudo apt-get install -qqy gcc-6

touch $IROOT/gcc-6.installed
