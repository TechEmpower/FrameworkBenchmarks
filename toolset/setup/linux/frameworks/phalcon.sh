#!/bin/bash

fw_depends php

RETCODE=$(fw_exists ${IROOT}/phalcon.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/phalcon.installed
  return 0; }

fw_get -O https://github.com/phalcon/cphalcon/archive/phalcon-v1.3.2.tar.gz
fw_untar phalcon-v1.3.2.tar.gz
cd cphalcon-phalcon-v1.3.2/build/64bits 
$PHP_HOME/bin/phpize
# For some reason we have to point to php-config 
# explicitly, it's not found by the prefix settings
./configure --prefix=$PHP_HOME --exec-prefix=$PHP_HOME \
  --with-php-config=$PHP_HOME/bin/php-config \
  --enable-phalcon --quiet
make --quiet
make install

echo "" > $IROOT/phalcon.installed

source $IROOT/phalcon.installed
