#!/bin/bash

RETCODE=$(fw_exists ${IROOT}/php-composer.installed)
[ ! "$RETCODE" == 0 ] || { return 0; }

fw_depends php
fw_get https://getcomposer.org/installer -O composer-installer.php

mkdir -p php-composer

cd php-composer

# Use the PHP and composer from our PHP_HOME directory and 
# COMPOSER_HOME directories (should be specified in frameworks 
# install.sh file)
${PHP_HOME}/bin/php composer-installer.php --install-dir=${COMPOSER_HOME}

cd ..
touch ${IROOT}/php-composer.installed