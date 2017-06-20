#!/bin/bash

fw_depends swift3

fw_installed ctls && return 0

echo "Installing ctls"

eval "$(curl -sL https://apt.vapor.sh)"
sudo apt-get install ctls

echo -e "export PATH=~/.local/bin:\$PATH" > $IROOT/ctls.installed

source $IROOT/ctls.installed
