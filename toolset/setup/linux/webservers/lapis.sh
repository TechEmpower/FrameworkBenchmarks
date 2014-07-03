#!/bin/bash
. ../toolset/setup/linux/bash_functions.sh

fw_exists /usr/local/bin/lapis
[ $? -ne 0 ] || { echo "lapis is installed!"; return 0; }

sudo apt-get install -y luarocks
sudo luarocks install http://github.com/leafo/lapis/raw/master/lapis-dev-1.rockspec