#!/bin/bash

# TODO double check this, it's logically different from original php code.
# Two path checks would both always run in php. In this code the check 
# for apc.so only happens if the check for php fails. Is that ok? 

RETCODE=$(fw_exists php.installed)
[ ! "$RETCODE" == 0 ] || { \
  echo "Moving PHP config files into place"; 
  sudo cp $FWROOT/config/php.ini /usr/local/lib/php.ini
  sudo cp $FWROOT/config/php-fpm.conf /usr/local/lib/php-fpm.conf
  return 0; }

fw_get http://php.net/distributions/php-5.5.17.tar.gz -O php-5.5.17.tar.gz
fw_untar php-5.5.17.tar.gz
ls
mv php-5.5.17 php
ls
cd php

./configure --prefix=$IROOT/php-5.5.17 --with-pdo-mysql --with-mysql --with-mcrypt --enable-intl --enable-mbstring --enable-fpm --with-fpm-user=www-data --with-fpm-group=www-data --with-openssl
make
make install
cd ..

cp $FWROOT/config/php.ini $IROOT/php-5.5.17/lib/php.ini
cp $FWROOT/config/php-fpm.conf $IROOT/php-5.5.17/lib/php-fpm.conf

# =======================
#
# Install the PHP extensions that our tests need
#    Install all of them here becuase our config file references 
#    all of these *.so
# ========================
echo PHP compilation finished, building modules

# Apc.so
$IROOT/php-5.5.17/bin/pecl config-set php_ini $IROOT/php-5.5.17/lib/php.ini
printf "\n" | $IROOT/php-5.5.17/bin/pecl install -f apc-beta

# yaf.so
printf "\n" | $IROOT/php-5.5.17/bin/pecl install -f yaf

# phalcon.so
#   The configure seems broken, does not respect prefix. If you 
#   update the value of PATH then it finds the prefix from `which php`
export PATH=$IROOT/php-5.5.17/bin:$IROOT/php-5.5.17/sbin:$PATH
git clone git://github.com/phalcon/cphalcon.git
cd cphalcon
git checkout phalcon-v1.3.2
cd build/64bits 
$IROOT/php-5.5.17/bin/phpize
./configure --prefix=$IROOT/php-5.5.17 --enable-phalcon
make
make install

# Clean up a bit
rm -rf $IROOT/php

touch $IROOT/php.installed
