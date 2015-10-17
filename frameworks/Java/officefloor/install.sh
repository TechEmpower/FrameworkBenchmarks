#!/bin/bash

# Propagate any failure
set -e

fw_depends java8

# Need maven 3.1.1 or higher (default for Ubuntu is 3.0.5)
sudo add-apt-repository "deb http://ppa.launchpad.net/natecarlson/maven3/ubuntu precise main"
sudo apt-get update
sudo apt-get -y --force-yes install maven3
if [ -e /usr/bin/mvn ]
then
    sudo rm -f /usr/bin/mvn
fi
sudo ln -s /usr/share/maven3/bin/mvn /usr/bin/mvn
