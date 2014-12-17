#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/hhvm.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

sudo add-apt-repository -y ppa:mapnik/v2.2.0
wget -O - http://dl.hhvm.com/conf/hhvm.gpg.key | sudo apt-key add -
echo deb http://dl.hhvm.com/ubuntu `lsb_release -sc` main | sudo tee /etc/apt/sources.list.d/hhvm.list
sudo apt-get update
sudo apt-get install -y hhvm

touch ${IROOT}/hhvm.installed