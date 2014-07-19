#!/bin/bash

RETCODE=$(fw_exists /usr/local/bin/lapis)
[ ! "$RETCODE" == 0 ] || { return 0; }

sudo apt-get install -y luarocks
sudo luarocks install http://github.com/leafo/lapis/raw/9e8b92bf40983a830312c1745c73db74db77192d/lapis-dev-1.rockspec
