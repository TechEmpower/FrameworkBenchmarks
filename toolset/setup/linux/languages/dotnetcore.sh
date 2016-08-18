#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/dotnetcore.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/dotnetcore.installed
  dotnet --info
  return 0; }

sudo apt-get install unzip libunwind8 -y
fw_get -O https://raw.githubusercontent.com/dotnet/cli/rel/1.0.0-preview2/scripts/obtain/dotnet-install.sh
chmod +x $IROOT/dotnet-install.sh

$IROOT/dotnet-install.sh
echo "PATH=$HOME/.dotnet:$PATH" > $IROOT/dotnetcore.installed

source $IROOT/dotnetcore.installed
dotnet --info
