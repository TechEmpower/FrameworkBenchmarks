#!/bin/bash

CPHALCON=$IROOT/cphalcon
RETCODE=$(fw_exists ${CPHALCON}.installed)
[ ! "$RETCODE" == 0 ] || { \
  # Load environment variables
  source $CPHALCON.installed
  return 0; }

fw_depends php

# phalcon.so
#   The configure seems broken, does not respect prefix. If you 
#   update the value of PATH then it finds the prefix from `which php`
git clone --depth=1 --branch=phalcon-v1.3.2 --single-branch \
  --quiet git://github.com/phalcon/cphalcon.git
cd cphalcon/build/64bits 
phpize
# For some reason we have to point to php-config 
# explicitly, it's not found by the prefix settings
./configure --prefix=$PHP_HOME --exec-prefix=$PHP_HOME \
  --with-php-config=$PHP_HOME/bin/php-config \
  --enable-phalcon --quiet
make --quiet
make install

# No variables to set with this framework.
touch $CPHALCON.installed
