#!/bin/bash

RETCODE=$(fw_exists play-1.2.5)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://downloads.typesafe.com/releases/play-1.2.5.zip -O play-1.2.5.zip
fw_unzip play-1.2.5.zip
mv play-1.2.5/play play-1.2.5/play1
