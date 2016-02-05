#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/maven.installed)
[ ! "$RETCODE" == 0 ] || { \
source $IROOT/maven.installed
return 0; }

sudo add-apt-repository "deb http://ppa.launchpad.net/natecarlson/maven3/ubuntu precise main"
sudo apt-get update
sudo apt-get -y --force-yes install maven3
if [ -e /usr/bin/mvn ]
then
    sudo rm -f /usr/bin/mvn
fi
sudo ln -s /usr/share/maven3/bin/mvn /usr/bin/mvn

mvn -version

touch $IROOT/maven.installed

source $IROOT/maven.installed
