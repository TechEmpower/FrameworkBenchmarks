#!/bin/bash

# TODO double check this, it's logically different from original php code.
# Two path checks would both always run in php. In this code the check 
# for apc.so only happens if the check for php fails. Is that ok? 

RETCODE=$(fw_exists /usr/local/bin/php)
[ ! "$RETCODE" == 0 ] || { \
  echo "Moving PHP config files into place"; 
  sudo cp $FWROOT/config/php.ini /usr/local/lib/php.ini
  sudo cp $FWROOT/config/php-fpm.conf /usr/local/lib/php-fpm.conf
  return 0; }

fw_get http://museum.php.net/php5/php-5.4.13.tar.gz
fw_untar php-5.4.13.tar.gz
cd php-5.4.13
./configure --with-pdo-mysql --with-mysql --with-mcrypt --enable-intl --enable-mbstring --enable-fpm --with-fpm-user=www-data --with-fpm-group=www-data --with-openssl
make
sudo make install

sudo cp $FWROOT/config/php.ini /usr/local/lib/php.ini
sudo cp $FWROOT/config/php-fpm.conf /usr/local/lib/php-fpm.conf

cd ..
RETCODE=$(fw_exists /usr/local/lib/php/extensions/no-debug-non-zts-20100525/apc.so)
[ ! "$RETCODE" == 0 ] || { return 0; }

cd php-5.4.13
printf "\n" | sudo pecl install apc-beta