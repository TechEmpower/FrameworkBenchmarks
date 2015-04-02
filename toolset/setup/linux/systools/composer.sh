#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/php-composer.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_depends php

PHP_HOME=${PHP_HOME:-${IROOT}/php-5.5.17}

mkdir -p php-composer
cd php-composer

fw_get https://getcomposer.org/installer -O composer-installer.php

# Use the PHP and composer from our PHP_HOME directory and 
# COMPOSER_HOME directories
${PHP_HOME}/bin/php composer-installer.php --install-dir=${COMPOSER_HOME}

cd ..
touch ${IROOT}/php-composer.installed