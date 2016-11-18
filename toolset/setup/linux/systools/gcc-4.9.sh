#!/bin/bash


RETCODE=$(fw_exists ${IROOT}/gcc-4.9.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/gcc-4.9.installed
  return 0; }

sudo add-apt-repository ppa:ubuntu-toolchain-r/test -y
sudo apt-get -yq update
sudo apt-get install -qqy gcc-4.9 g++-4.9

touch $IROOT/gcc-4.9.installed
