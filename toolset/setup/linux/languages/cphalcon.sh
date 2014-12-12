#!/bin/bash

# phalcon.so
#   The configure seems broken, does not respect prefix. If you 
#   update the value of PATH then it finds the prefix from `which php`

RETCODE=$(fw_exists ${IROOT}/cphalcon.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

export PATH=$IROOT/php-5.5.17/bin:$IROOT/php-5.5.17/sbin:$PATH
git clone git://github.com/phalcon/cphalcon.git
cd cphalcon
git checkout phalcon-v1.3.2
cd build/64bits 
$IROOT/php-5.5.17/bin/phpize
./configure --prefix=$IROOT/php-5.5.17 --enable-phalcon
make
make install

touch ${IROOT}/cphalcon.installed