#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/phalcon.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/phalcon.installed
  return 0; }

PHP_VERSION="5.5.17"
PHP_HOME=$IROOT/php-$PHP_VERSION

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

echo "export PHP_HOME=${PHP_HOME}" > $IROOT/phalcon.installed
echo -e "export PATH=\$PHP_HOME/bin:\$PHP_HOME/sbin:\$PATH" >> $IROOT/phalcon.installed

echo "export PHALCON_HOME=${PHALCON_HOME}" > $IROOT/phalcon.installed
echo -e "export PATH=\$PHALCON_HOME:\$PATH" >> $IROOT/phalcon.installed


source $IROOT/phalcon.installed
