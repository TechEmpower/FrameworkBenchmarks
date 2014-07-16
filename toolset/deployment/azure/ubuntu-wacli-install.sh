#!/bin/bash  
#
# Installs Windows Azure Command-Line Tools on Ubuntu.
#
# http://www.windowsazure.com/en-us/develop/nodejs/how-to-guides/command-line-tools/
# http://www.windowsazure.com/en-us/manage/linux/other-resources/command-line-tools/
# https://github.com/WindowsAzure/azure-sdk-tools-xplat
#
# Note: This script was tested on a brand-new Ubuntu Server 12.04 VM.
#
set -o nounset -o errexit

# Install node from community repository.
# https://github.com/joyent/node/wiki/Installing-Node.js-via-package-manager#ubuntu-mint
# http://stackoverflow.com/a/16303380/376366
sudo apt-get update
sudo apt-get install software-properties-common python-software-properties python g++ make
sudo add-apt-repository ppa:chris-lea/node.js
sudo apt-get update
sudo apt-get install nodejs

# Install Windows Azure Command-Line Tools.
sudo npm install azure-cli -g
azure
