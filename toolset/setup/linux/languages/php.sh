#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/php.installed)
[ ! "$RETCODE" == 0 ] || { \
  echo "Moving PHP config files into place"; 
  sudo cp $FWROOT/config/php.ini /usr/local/lib/php.ini
  sudo cp $FWROOT/config/php-fpm.conf /usr/local/lib/php-fpm.conf
  source $IROOT/php.installed
  return 0; }

VERSION="5.5.17"
PHP_HOME=$IROOT/php-$VERSION

# Precaution, unlikely to happen.
rm -rf $IROOT/php PHP_HOME cphalcon

fw_get -o php-${VERSION}.tar.gz http://php.net/distributions/php-${VERSION}.tar.gz
fw_untar php-${VERSION}.tar.gz
mv php-${VERSION} php
cd php

echo "Configuring PHP quietly..."
./configure --prefix=$PHP_HOME --with-pdo-mysql \
  --with-mysql --with-mcrypt --enable-intl --enable-mbstring \
  --enable-fpm --with-fpm-user=testrunner --with-fpm-group=testrunner \
  --with-openssl --with-mysqli --with-zlib --enable-opcache --quiet
echo "Making PHP quietly..."
make --quiet
echo "Installing PHP quietly"
make --quiet install
cd ..

cp $FWROOT/config/php.ini $PHP_HOME/lib/php.ini
cp $FWROOT/config/php-fpm.conf $PHP_HOME/lib/php-fpm.conf

# =======================
#
# Install the PHP extensions that our tests need
#    Install all of them here becuase our config file references 
#    all of these *.so
# ========================
echo PHP compilation finished, installing extensions

$PHP_HOME/bin/pecl channel-update pecl.php.net
# Apc.so
$PHP_HOME/bin/pecl config-set php_ini $PHP_HOME/lib/php.ini
printf "\n" | $PHP_HOME/bin/pecl -q install -f redis

# yaf.so
printf "\n" | $PHP_HOME/bin/pecl -q install -f yaf

# phalcon.so
#   The configure seems broken, does not respect prefix. If you 
#   update the value of PATH then it finds the prefix from `which php`

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

# mongo.so
printf "\n" | $PHP_HOME/bin/pecl -q install -f mongo

# Clean up a bit
rm -rf $IROOT/php

echo "export PHP_HOME=${PHP_HOME}" > $IROOT/php.installed
echo -e "export PATH=\$PHP_HOME/bin:\$PHP_HOME/sbin:\$PATH" >> $IROOT/php.installed

source $IROOT/php.installed
