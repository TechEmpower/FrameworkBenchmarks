. ../toolset/setup/linux/bash_functions.sh

fw_exists play-2.2.0
[ $? -ne 0 ] || { echo "Play version 2 is installed!"; return 0; }

fw_get http://downloads.typesafe.com/play/2.2.0/play-2.2.0.zip
fw_unzip play-2.2.0.zip
