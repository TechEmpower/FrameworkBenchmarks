#!/bin/bash

RETCODE=$(fw_exists play-1.2.5/modules/siena-2.0.6)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_depends play1
yes | play-1.2.5/play1 install siena-2.0.6