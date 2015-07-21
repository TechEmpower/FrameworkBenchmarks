#!/bin/bash

export PHP_HOME=${IROOT}/php-5.5.17
export COMPOSER_HOME=${IROOT}/php-composer
export PHP_FPM=$PHP_HOME/sbin/php-fpm
export NGINX_HOME=${IROOT}/nginx

fw_depends php nginx composer

${PHP_HOME}/bin/php ${COMPOSER_HOME}/composer.phar install \
  --no-interaction --working-dir $TROOT \
  --no-progress --optimize-autoloader 
