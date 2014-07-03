#!/bin/bash
. ../toolset/setup/linux/bash_functions.sh

# TODO double check this, it's logically different from original python code.
# Two path checks would both always run in python. In this code the check 
# for apc.so only happens if the check for php fails. Is that ok? 

fw_exists /usr/local/bin/php
[ $? -ne 0 ] || { \
  echo "PHP is installed!"; 
  sudo cp ../config/php.ini /usr/local/lib/php.ini
  sudo cp ../config/php-fpm.conf /usr/local/lib/php-fpm.conf
  return 0; }

fw_get http://museum.php.net/php5/php-5.4.13.tar.gz
fw_untar php-5.4.13.tar.gz
cd php-5.4.13
./configure --with-pdo-mysql --with-mysql --with-mcrypt --enable-intl --enable-mbstring --enable-fpm --with-fpm-user=www-data --with-fpm-group=www-data --with-openssl
make
sudo make install

cd ..
fw_exists /usr/local/lib/php/extensions/no-debug-non-zts-20100525/apc.so
[ $? -ne 0 ] || { echo "PHP is installed!"; return 0; }

cd php-5.4.13
printf "\n" | sudo pecl install apc-beta

sudo cp ../config/php.ini /usr/local/lib/php.ini
sudo cp ../config/php-fpm.conf /usr/local/lib/php-fpm.conf
