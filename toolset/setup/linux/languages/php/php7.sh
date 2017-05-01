#!/bin/bash

fw_installed php7 && return 0

VERSION="7.1.4"
PHP_HOME=$IROOT/php-$VERSION

fw_get -o php-${VERSION}.tar.gz http://php.net/distributions/php-${VERSION}.tar.gz
fw_untar php-${VERSION}.tar.gz
mv php-${VERSION} php7
cd php7

echo "Configuring PHP quietly..."
./configure --prefix=$PHP_HOME --with-pdo-mysql \
  --with-mcrypt --enable-intl --enable-mbstring \
  --enable-fpm --with-openssl --with-mysqli \
  --with-zlib --enable-opcache --quiet
echo "Making PHP quietly..."
make --quiet
echo "Installing PHP quietly"
make --quiet install
cd ..

# Disable yaf and phalcon, for most PHP frameworks
# (there is a similar line to enable the frameworks in their respective setup files)
sed -i 's|^extension=yaf.so|;extension=yaf.so|g' $FWROOT/toolset/setup/linux/languages/php/php.ini
sed -i 's|^extension=phalcon.so|;extension=phalcon.so|g' $FWROOT/toolset/setup/linux/languages/php/php.ini

# Enable the correct Mongo DB plugin for PHP 7
sed -i 's|^extension=mongo.so|;extension=mongo.so|g' $FWROOT/toolset/setup/linux/languages/php/php.ini
sed -i 's|;extension=mongodb.so|extension=mongodb.so|g' $FWROOT/toolset/setup/linux/languages/php/php.ini

cp $FWROOT/toolset/setup/linux/languages/php/php.ini $PHP_HOME/lib/php.ini
cp $FWROOT/toolset/setup/linux/languages/php/php-fpm.conf $PHP_HOME/lib/php-fpm.conf

# =======================
#
# Install the PHP extensions that our tests need
#    Install all of them here becuase our config file references
#    all of these *.so
# ========================
echo PHP7 compilation finished, installing extensions

$PHP_HOME/bin/pecl channel-update pecl.php.net
# Apc.so
$PHP_HOME/bin/pecl config-set php_ini $PHP_HOME/lib/php.ini

#printf "\n" | $PHP_HOME/bin/pecl -q install -f redis-3.0.0

#removed phalcon install - separate to toolset/setup/linux/frameworks

# yaf.so - get working for php7 - also separate
#printf "\n" | $PHP_HOME/bin/pecl -q install -f yaf

# mongodb.so - mongo.so deprecated in php7 use mongodb.so
printf "\n" | $PHP_HOME/bin/pecl -q install -f mongodb

# Clean up a bit
rm -rf $IROOT/php7

echo "export PHP_HOME=${PHP_HOME}" > $IROOT/php7.installed
echo -e "export PATH=\$PHP_HOME/bin:\$PHP_HOME/sbin:\$PATH" >> $IROOT/php7.installed

source $IROOT/php7.installed
