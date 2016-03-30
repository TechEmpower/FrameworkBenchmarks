#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/ant.installed)
[ ! "$RETCODE" == 0 ] || { \
source $IROOT/ant.installed
return 0; }

sudo apt-get update
sudo apt-get -y --force-yes install ant

echo "export PATH=/usr/share/ant/bin:\$PATH" > $IROOT/ant.installed

source $IROOT/ant.installed

ant -version
