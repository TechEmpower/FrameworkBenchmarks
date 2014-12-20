#!/bin/bash

fw_depends php nginx composer

export PHP_HOME=${IROOT}/php-5.5.17
${PHP_HOME}/bin/php $IROOT/composer.phar install \
  --no-interaction --working-dir $TROOT \
  --no-progress --optimize-autoloader 

