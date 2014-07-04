#!/bin/bash

fw_exists /usr/local/bin/lapis
[ $? -ne 0 ] || { return 0; }

sudo apt-get install -y luarocks
sudo luarocks install http://github.com/leafo/lapis/raw/master/lapis-dev-1.rockspec