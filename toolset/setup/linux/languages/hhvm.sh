#!/bin/bash

fw_installed hhvm && return 0

# TODO: Someday move away from apt-get

# Uninstall hhvm if it was previously installed.
if [ $(dpkg-query -W -f='${Status}' hhvm 2>/dev/null | grep -c "ok installed") -eq 1 ];
then
  sudo apt-get remove -y hhvm
fi

fw_get http://dl.hhvm.com/conf/hhvm.gpg.key | sudo apt-key add -

# hhvm 3.24 causes most of our hhvm-using test implementations to segmentation
# fault when connecting to the mysql database (the crash happens right as they
# try to specify utf8 as the charset).  hhvm 3.21 doesn't have this problem.
echo deb [arch=amd64] http://dl.hhvm.com/ubuntu `lsb_release -sc`-lts-3.21 main | sudo tee /etc/apt/sources.list.d/hhvm.list
sudo apt-get update
sudo apt-get install -y hhvm

sudo service hhvm stop

echo "" > $IROOT/hhvm.installed

source $IROOT/hhvm.installed
