#!/bin/bash

COMPOSER_HOME=$IROOT/php-composer
RETCODE=$(fw_exists ${COMPOSER_HOME}.installed)
[ ! "$RETCODE" == 0 ] || { \
  source $COMPOSER_HOME.installed
  return 0; }

fw_depends php

mkdir -p php-composer
cd php-composer

fw_get https://getcomposer.org/installer -O composer-installer.php

# Use the PHP and composer from our PHP_HOME directory and 
# COMPOSER_HOME directories
${PHP_HOME}/bin/php composer-installer.php --install-dir=${COMPOSER_HOME}

cd ..

echo "export COMPOSER_HOME=${COMPOSER_HOME}" > $COMPOSER_HOME.installed
echo -e "${PHP_HOME}/bin/php ${COMPOSER_HOME}/composer.phar install --no-interaction --working-dir \$TROOT --no-progress --optimize-autoloader" >> $COMPOSER_HOME.installed

source $COMPOSER_HOME.installed
