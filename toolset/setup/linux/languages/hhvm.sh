#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/hhvm.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/hhvm.installed
  return 0; }

# TODO: Someday move away from apt-get
fw_get http://dl.hhvm.com/conf/hhvm.gpg.key | sudo apt-key add -
echo deb http://dl.hhvm.com/ubuntu `lsb_release -sc` main | sudo tee /etc/apt/sources.list.d/hhvm.list
sudo apt-get update
sudo apt-get install -y hhvm

sudo service stop hhvm

echo "" > $IROOT/hhvm.installed

source $IROOT/hhvm.installed
