. ../toolset/setup/linux/bash_functions.sh

fw_exists jester
[ $? -ne 0 ] || { echo "Jester is installed!"; return 0; }

git clone git://github.com/dom96/jester.git jester/jester