#!/bin/bash

fw_depends php nginx composer

export COMPOSER_HOME=${IROOT}/php-composer
PHP_HOME=${IROOT}/php-5.5.17
${PHP_HOME}/bin/php $COMPOSER_HOME/composer.phar install \
  --no-interaction --working-dir $TROOT \
  --no-progress --optimize-autoloader 