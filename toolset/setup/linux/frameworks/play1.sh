#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/play-1.2.5.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://downloads.typesafe.com/releases/play-1.2.5.zip -O
fw_unzip play-1.2.5.zip

touch ${IROOT}/play-1.2.5.installed