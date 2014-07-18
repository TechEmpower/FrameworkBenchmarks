#!/bin/bash

RETCODE=$(fw_exists /usr/local/lib/php/extensions/no-debug-non-zts-20100525/yaf.so)
[ ! "$RETCODE" == 0 ] || { \
  echo "Enabling yaf in PHP configuration...";
  sudo cp ../config/php.ini /usr/local/lib/php.ini
  sudo cp ../config/php-fpm.conf /usr/local/lib/php-fpm.conf
  return 0; }

fw_depends php
sudo pecl install -f yaf
sudo cp ../config/php.ini /usr/local/lib/php.ini
sudo cp ../config/php-fpm.conf /usr/local/lib/php-fpm.conf

