#!/bin/bash
. ../toolset/setup/linux/bash_functions.sh

fw_exists play-1.2.5
[ $? -ne 0 ] || { echo "Play version 1 is installed!"; return 0; }

fw_get http://downloads.typesafe.com/releases/play-1.2.5.zip -O play-1.2.5.zip
fw_unzip play-1.2.5.zip
mv play-1.2.5/play play-1.2.5/play1
