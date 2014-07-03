. ../toolset/setup/linux/bash_functions.sh


fw_exists play-1.2.5/modules/siena-2.0.6
[ $? -ne 0 ] || { echo "Siena is installed!"; return 0; }

fw_depends play1
yes | play-1.2.5/play1 install siena