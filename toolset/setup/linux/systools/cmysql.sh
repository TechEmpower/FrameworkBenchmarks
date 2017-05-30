#!/bin/bash

fw_depends swift3

fw_installed cmysql && return 0

echo "Installing cmysql"

eval "$(curl -sL https://apt.vapor.sh)"
sudo apt-get install cmysql

echo -e "export PATH=~/.local/bin:\$PATH" > $IROOT/cmysql.installed

source $IROOT/cmysql.installed
