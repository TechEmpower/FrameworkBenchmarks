#!/bin/bash

fw_installed maven && return 0

sudo add-apt-repository "deb http://ppa.launchpad.net/natecarlson/maven3/ubuntu precise main"
sudo apt-get update
sudo apt-get -y --force-yes install maven3

echo "export PATH=/usr/share/maven3/bin:\$PATH" > $IROOT/maven.installed

source $IROOT/maven.installed

mvn -version
