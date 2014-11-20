#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/lua5.1.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

# Eventually, we should also install lua5.2 and luajit
#
# At the moment they seem to cause issues with lapis 
# being able to compile. Since no Lua test is using 
# either luajit or lua5.2 at the moment I have just
# left them out
sudo apt-get install -y lua5.1 luarocks

touch ${IROOT}/lua5.1.installed