#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/dotnetcore.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/dotnetcore.installed
  dotnet --info
  return 0; }

sudo sh -c 'echo "deb [arch=amd64] https://apt-mo.trafficmanager.net/repos/dotnet-release/ trusty main" > /etc/apt/sources.list.d/dotnetdev.list' 
sudo apt-key adv --keyserver apt-mo.trafficmanager.net --recv-keys 417A0893
sudo apt-get update

sudo apt-get install dotnet-dev-1.0.0-preview2-003131 -y
echo "PATH=$HOME/.dotnet:$PATH" > $IROOT/dotnetcore.installed

source $IROOT/dotnetcore.installed
dotnet --info
