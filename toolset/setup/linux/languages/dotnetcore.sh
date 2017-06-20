#!/bin/bash

fw_installed dotnetcore && return 0

sudo sh -c 'echo "deb [arch=amd64] https://apt-mo.trafficmanager.net/repos/dotnet-release/ trusty main" > /etc/apt/sources.list.d/dotnetdev.list'
sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys 417A0893
sudo apt-get update

sudo apt-get install -y dotnet-dev-2.0.0-preview1-005977
echo "PATH=$HOME/.dotnet:$PATH" > $IROOT/dotnetcore.installed

source $IROOT/dotnetcore.installed
dotnet --info
