. ../toolset/setup/linux/bash_functions.sh

fw_exists /usr/local/bin/urweb
[ $? -ne 0 ] || { echo "urweb is installed!"; return 0; }

hg clone http://hg.impredicative.com/urweb
cd urweb
./autogen.sh
./configure
make
sudo make install
