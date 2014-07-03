#!/bin/bash
. ../toolset/setup/linux/bash_functions.sh

fw_exists /usr/share/ringojs
[ $? -ne 0 ] || { echo "RingoJS is installed!"; return 0; }

fw_get http://www.ringojs.org/downloads/ringojs_0.10-1_all.deb
sudo apt-get install -y jsvc
sudo dpkg -i ringojs_0.10-1_all.deb

rm ringojs_0.10-1_all.deb
