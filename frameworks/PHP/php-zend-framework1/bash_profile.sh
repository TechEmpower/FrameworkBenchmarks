#!/bin/bash

export PHP_HOME=${IROOT}/php-5.5.17

export PHP_FPM=$PHP_HOME/sbin/php-fpm

export COMPOSER_HOME=${IROOT}/php-composer

export PATH="$COMPOSER_HOME:$PHP_HOME/bin:$PHP_HOME/sbin:$PATH"
