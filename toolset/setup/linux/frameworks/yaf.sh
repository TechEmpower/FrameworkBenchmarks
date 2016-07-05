#!/bin/bash

fw_depends php5

RETCODE=$(fw_exists ${IROOT}/yaf.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/yaf.installed
  return 0; }

# Enable the PHP yaf extension
sed -i 's|;extension=yaf.so|extension=yaf.so|g' $PHP_HOME/lib/php.ini
printf "\n" | $PHP_HOME/bin/pecl -q install -f yaf-2.2.9

echo "" > $IROOT/yaf.installed

source $IROOT/yaf.installed
