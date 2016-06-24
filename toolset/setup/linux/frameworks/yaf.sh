#!/bin/bash

fw_depends php

RETCODE=$(fw_exists ${IROOT}/yaf.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/yaf.installed
  return 0; }

printf "\n" | $PHP_HOME/bin/pecl -q install -f yaf-2.2.9

echo "" > $IROOT/yaf.installed

source $IROOT/yaf.installed
