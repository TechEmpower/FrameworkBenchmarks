#!/bin/bash
. ../toolset/setup/linux/bash_functions.sh

fw_exists nawak
[ $? -ne 0 ] || { echo "Nawak is installed!"; return 0; }

git clone git://github.com/idlewan/nawak.git nawak/nawak