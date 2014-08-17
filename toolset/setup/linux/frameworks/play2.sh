#!/bin/bash

RETCODE=$(fw_exists play-2.2.0)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_get http://downloads.typesafe.com/play/2.2.0/play-2.2.0.zip -O play-2.2.0.zip
fw_unzip play-2.2.0.zip