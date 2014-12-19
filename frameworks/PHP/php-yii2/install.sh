#!/bin/bash

fw_depends php nginx composer

PHP_HOME=${IROOT}/php-5.5.17
cd $TROOT && ${PHP_HOME}/bin/php $IROOT/composer.phar --no-interaction install
