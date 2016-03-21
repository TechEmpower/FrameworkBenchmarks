#!/bin/bash

RETCODE=$(fw_exists $IROOT/mono.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $IROOT/mono.installed
  return 0; }
  
# Add source for prepackaged binaries
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF
echo "deb http://download.mono-project.com/repo/debian wheezy main" | sudo tee /etc/apt/sources.list.d/mono-xamarin.list
sudo apt-get update
sudo apt-get install -y mono-complete mono-fastcgi-server

# Mono installs to PATH (/usr/bin/mono) :( so no env vars
echo "# Mono installed to PATH /usr/bin " > $IROOT/mono.installed

source $IROOT/mono.installed
