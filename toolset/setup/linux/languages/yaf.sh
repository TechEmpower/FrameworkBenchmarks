. ../toolset/setup/linux/bash_functions.sh

fw_exists /usr/local/lib/php/extensions/no-debug-non-zts-20100525/yaf.so
[ $? -ne 0 ] || { \
  echo "yaf is installed, enabling in configuration...";
  sudo cp ../config/php.ini /usr/local/lib/php.ini
  sudo cp ../config/php-fpm.conf /usr/local/lib/php-fpm.conf
  echo "yaf is installed!"
  return 0; }

fw_depends php
sudo pecl install -f yaf
sudo cp ../config/php.ini /usr/local/lib/php.ini
sudo cp ../config/php-fpm.conf /usr/local/lib/php-fpm.conf

