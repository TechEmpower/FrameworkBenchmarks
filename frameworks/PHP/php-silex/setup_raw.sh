#!/bin/bash
export PHP_HOME=${IROOT}/php-5.5.17
export PHP_FPM=$PHP_HOME/sbin/php-fpm
export COMPOSER_HOME=${IROOT}/php-composer
export NGINX_HOME=${IROOT}/nginx

sed -i 's|172.16.98.120|'"${DBHOST}"'|g' web/index_raw.php
sed -i 's|".*\FrameworkBenchmarks/php-silex|"'"${TROOT}"'|g' deploy/php-silex
sed -i 's|Directory .*/FrameworkBenchmarks/php-silex|Directory '"${TROOT}"'|g' deploy/php-silex
sed -i 's|root .*/FrameworkBenchmarks/php-silex|root '"${TROOT}"'|g' deploy/nginx_raw.conf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx_raw.conf

export PATH="$COMPOSER_HOME:$PHP_HOME/bin:$PHP_HOME/sbin:$PATH"

$PHP_FPM --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
$NGINX_HOME/sbin/nginx -c $TROOT/deploy/nginx_raw.conf