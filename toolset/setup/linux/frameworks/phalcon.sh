#!/bin/bash

fw_depends php5

RETCODE=$(fw_exists ${IROOT}/phalcon.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/phalcon.installed
  return 0; }

# Enable the PHP phalcon extension
sed -i 's|;extension=phalcon.so|extension=phalcon.so|g' $PHP_HOME/lib/php.ini

fw_get -O https://github.com/phalcon/cphalcon/archive/phalcon-v2.0.13.tar.gz
fw_untar phalcon-v2.0.13.tar.gz
cd cphalcon-phalcon-v2.0.13/build/64bits 
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
