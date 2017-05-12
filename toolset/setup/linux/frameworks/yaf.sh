#!/bin/bash

fw_depends php5

fw_installed yaf && return 0

VERSION="2.3.5"
# Enable the PHP yaf extension
sed -i 's|;extension=yaf.so|extension=yaf.so|g' $PHP_HOME/lib/php.ini
# pecl install make fail
#printf "\n" | $PHP_HOME/bin/pecl install -f yaf-2.3.5

fw_get -O http://pecl.php.net/get/yaf-${VERSION}.tgz
fw_untar yaf-${VERSION}.tgz
cd yaf-${VERSION}
phpize
./configure
make install

echo "" > $IROOT/yaf.installed

source $IROOT/yaf.installed