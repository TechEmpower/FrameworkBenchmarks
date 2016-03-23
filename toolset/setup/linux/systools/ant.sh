#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/ant.installed)
[ ! "$RETCODE" == 0 ] || { \
source $IROOT/ant.installed
return 0; }

sudo apt-get update
sudo apt-get -y --force-yes install ant

if [ -e /usr/bin/ant ]
then
    sudo rm -f /usr/bin/ant
fi
sudo ln -s /usr/share/ant/bin/ant /usr/bin/ant

ant -version

touch $IROOT/ant.installed

source $IROOT/ant.installed
