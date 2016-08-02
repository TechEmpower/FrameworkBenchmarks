#!/bin/bash

fw_depends php7

RETCODE=$(fw_exists ${IROOT}/phalcon.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/phalcon.installed
  return 0; }

# Enable the PHP phalcon extension
sed -i 's|;extension=phalcon.so|extension=phalcon.so|g' $PHP_HOME/lib/php.ini

fw_get -O https://github.com/phalcon/cphalcon/archive/v3.0.0.tar.gz
fw_untar v3.0.0.tar.gz
cd cphalcon-3.0.0/build
./install

echo "" > $IROOT/phalcon.installed

source $IROOT/phalcon.installed
