#!/bin/bash

fw_installed composer && return 0

COMPOSER_HOME=$IROOT/php-composer

mkdir -p $COMPOSER_HOME
cd $COMPOSER_HOME

fw_get -o composer-installer.php https://getcomposer.org/installer

# Use the PHP and composer from our PHP_HOME directory and 
# COMPOSER_HOME directories
#
# NOTE: if you decide to update the composer version, be sure to test ALL of the frameworks
# that use composer, as some of them have been known to break on newer versions of composer.
php composer-installer.php --install-dir=${COMPOSER_HOME} --version=1.0.0

cd ..

echo "export COMPOSER_HOME=${COMPOSER_HOME}" > $IROOT/composer.installed
echo -e "php \$COMPOSER_HOME/composer.phar install --no-interaction --working-dir \$TROOT --no-progress --optimize-autoloader" >> $IROOT/composer.installed

source $IROOT/composer.installed
