#!/bin/bash

RETCODE=$(fw_exists $IROOT/xsp.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

echo "Installing XSP"
sudo apt-get -y install mono-xsp4 mono-fastcgi-server4

touch $IROOT/xsp.installed
