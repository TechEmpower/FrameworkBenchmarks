#!/bin/bash

export PHP_HOME=${IROOT}/php-5.5.17
export COMPOSER_HOME=${IROOT}/php-composer

fw_depends php nginx composer

${PHP_HOME}/bin/php ${COMPOSER_HOME}/composer.phar install \
  --no-interaction --working-dir ${TROOT} \
  --no-progress --optimize-autoloader 
php artisan optimize --force
  
