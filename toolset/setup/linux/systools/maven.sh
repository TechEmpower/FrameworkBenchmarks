#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/maven.installed)
[ ! "$RETCODE" == 0 ] || { \
source $IROOT/maven.installed
return 0; }

sudo add-apt-repository "deb http://ppa.launchpad.net/natecarlson/maven3/ubuntu precise main"
sudo apt-get update
sudo apt-get -y --force-yes install maven3

echo "export PATH=/usr/share/maven3/bin:\$PATH" > $IROOT/maven.installed

source $IROOT/maven.installed

mvn -version
