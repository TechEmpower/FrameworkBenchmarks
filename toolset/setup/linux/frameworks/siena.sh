#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/siena-2.0.6.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_depends play1
yes | ${IROOT}/play-1.2.5/play install siena-2.0.6

touch ${IROOT}/siena-2.0.6.installed