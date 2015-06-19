#!/bin/bash

# TODO double check this, it's logically different from original php code.
# Two path checks would both always run in php. In this code the check 
# for apc.so only happens if the check for php fails. Is that ok? 

RETCODE=$(fw_exists ${IROOT}/php.installed)
[ ! "$RETCODE" == 0 ] || { \
  echo "Moving PHP config files into place"; 
  sudo cp $FWROOT/config/php.ini /usr/local/lib/php.ini
  sudo cp $FWROOT/config/php-fpm.conf /usr/local/lib/php-fpm.conf
  source $IROOT/php.installed
  return 0; }

VERSION="5.5.17"
PHP_HOME=$IROOT/php-$VERSION

fw_get http://php.net/distributions/php-${VERSION}.tar.gz -o php-${VERSION}.tar.gz
fw_untar php-${VERSION}.tar.gz
mv php-${VERSION} php
cd php

echo "Configuring PHP quietly..."
./configure --prefix=$IROOT/php-${VERSION} --with-pdo-mysql \
  --with-mysql --with-mcrypt --enable-intl --enable-mbstring \
  --enable-fpm --with-fpm-user=testrunner --with-fpm-group=testrunner \
  --with-openssl --with-mysqli --with-zlib --enable-opcache --quiet
echo "Making PHP quietly..."
make --quiet
echo "Installing PHP quietly"
make --quiet install
cd ..

cp $FWROOT/config/php.ini $IROOT/php-${VERSION}/lib/php.ini
cp $FWROOT/config/php-fpm.conf $IROOT/php-${VERSION}/lib/php-fpm.conf

# =======================
#
# Install the PHP extensions that our tests need
#    Install all of them here becuase our config file references 
#    all of these *.so
# ========================
echo PHP compilation finished, installing extensions

$IROOT/php-${VERSION}/bin/pecl channel-update pecl.php.net
# Apc.so
$IROOT/php-${VERSION}/bin/pecl config-set php_ini $IROOT/php-${VERSION}/lib/php.ini
printf "\n" | $IROOT/php-${VERSION}/bin/pecl -q install -f redis

# yaf.so
printf "\n" | $IROOT/php-${VERSION}/bin/pecl -q install -f yaf

# phalcon.so
#   The configure seems broken, does not respect prefix. If you 
#   update the value of PATH then it finds the prefix from `which php`
git clone --depth=1 --branch=phalcon-v1.3.2 --single-branch \
  --quiet git://github.com/phalcon/cphalcon.git
cd cphalcon/build/64bits 
$PHP_HOME/bin/phpize
# For some reason we have to point to php-config 
# explicitly, it's not found by the prefix settings
./configure --prefix=$PHP_HOME --exec-prefix=$PHP_HOME \
  --with-php-config=$PHP_HOME/bin/php-config \
  --enable-phalcon --quiet
make --quiet
make install

# mongo.so
printf "\n" | $IROOT/php-${VERSION}/bin/pecl -q install -f mongo

# Clean up a bit
rm -rf $IROOT/php

echo "export PHP_HOME=${IROOT}/php-5.5.17" > $IROOT/php.installed
echo -e "export PATH=${PHP_HOME}/bin:$PHP_HOME/sbin:\$PATH" >> $IROOT/php.installed

source $IROOT/php.installed
