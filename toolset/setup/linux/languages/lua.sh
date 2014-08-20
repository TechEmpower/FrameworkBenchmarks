#!/bin/bash

RETCODE=$(fw_exists lua.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

sudo apt-get install -y lua5.1 lua5.2 luajit luarocks

touch lua.installed