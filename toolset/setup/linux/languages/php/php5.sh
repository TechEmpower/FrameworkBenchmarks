#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/php5.installed)
[ ! "$RETCODE" == 0 ] || { \
  echo "Moving PHP config files into place"; 
  sudo cp $FWROOT/toolset/setup/linux/languages/php/php.ini /usr/local/lib/php.ini
  sudo cp $FWROOT/toolset/setup/linux/languages/php/php-fpm.conf /usr/local/lib/php-fpm.conf
  source $IROOT/php5.installed
  return 0; }

VERSION="5.6.30"
PHP_HOME=$IROOT/php-$VERSION

# Precaution, unlikely to happen.
rm -rf $IROOT/php PHP_HOME cphalcon

fw_get -o php-${VERSION}.tar.gz http://php.net/distributions/php-${VERSION}.tar.gz
fw_untar php-${VERSION}.tar.gz
mv php-${VERSION} php5
cd php5

echo "Configuring PHP5 quietly..."
./configure --prefix=$PHP_HOME --with-pdo-mysql \
  --with-mysql --with-mcrypt --enable-intl --enable-mbstring \
  --enable-fpm --with-openssl --with-mysqli --with-zlib \
  --enable-opcache --quiet
echo "Making PHP5 quietly..."
make --quiet
echo "Installing PHP5 quietly"
make --quiet install
cd ..

# Disable yaf and phalcon, for most PHP frameworks 
# (there is a similar line to enable the frameworks in their respective setup files)
sed -i 's|^extension=yaf.so|;extension=yaf.so|g' $FWROOT/toolset/setup/linux/languages/php/php.ini
sed -i 's|^extension=phalcon.so|;extension=phalcon.so|g' $FWROOT/toolset/setup/linux/languages/php/php.ini

# Enable the correct Mongo DB plugin for PHP 5
sed -i 's|^extension=mongodb.so|;extension=mongodb.so|g' $FWROOT/toolset/setup/linux/languages/php/php.ini
sed -i 's|;extension=mongo.so|extension=mongo.so|g' $FWROOT/toolset/setup/linux/languages/php/php.ini

cp $FWROOT/toolset/setup/linux/languages/php/php.ini $PHP_HOME/lib/php.ini
cp $FWROOT/toolset/setup/linux/languages/php/php-fpm.conf $PHP_HOME/lib/php-fpm.conf

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
printf "\n" | $PHP_HOME/bin/pecl -q install -f redis-2.2.5

# mongo.so
printf "\n" | $PHP_HOME/bin/pecl -q install -f mongo

# Clean up a bit
rm -rf $IROOT/php

echo "export PHP_HOME=${PHP_HOME}" > $IROOT/php5.installed
echo -e "export PATH=\$PHP_HOME/bin:\$PHP_HOME/sbin:\$PATH" >> $IROOT/php5.installed

source $IROOT/php5.installed
