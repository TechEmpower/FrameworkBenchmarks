#!/bin/bash

fw_installed dotnetcore && return 0

sudo apt-get purge -y --auto-remove dotnet-dev-2.0.0-* || true
sudo apt-get purge -y --auto-remove dotnet-sdk-2.0.0-preview* || true

# https://www.microsoft.com/net/core#linuxubuntu
curl https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > microsoft.gpg
sudo mv microsoft.gpg /etc/apt/trusted.gpg.d/microsoft.gpg
sudo sh -c 'echo "deb [arch=amd64] https://packages.microsoft.com/repos/microsoft-ubuntu-trusty-prod trusty main" > /etc/apt/sources.list.d/dotnetdev.list'
sudo apt-get update
sudo apt-get install -y dotnet-sdk-2.0.0

echo "PATH=$HOME/.dotnet:$PATH" > $IROOT/dotnetcore.installed

source $IROOT/dotnetcore.installed
dotnet --info
