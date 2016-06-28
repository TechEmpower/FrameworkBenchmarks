#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/composer.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $IROOT/composer.installed
  return 0; }

COMPOSER_HOME=$IROOT/php-composer

mkdir -p $COMPOSER_HOME
cd $COMPOSER_HOME

fw_get -o composer-installer.php https://getcomposer.org/installer

# Use the PHP and composer from our PHP_HOME directory and 
# COMPOSER_HOME directories
php composer-installer.php --install-dir=${COMPOSER_HOME}

cd ..

echo "export COMPOSER_HOME=${COMPOSER_HOME}" > $IROOT/composer.installed
echo -e "php \$COMPOSER_HOME/composer.phar install --no-interaction --working-dir \$TROOT --no-progress --optimize-autoloader" >> $IROOT/composer.installed

source $IROOT/composer.installed
