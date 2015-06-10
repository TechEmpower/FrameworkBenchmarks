#!/bin/bash
export PHP_HOME=${IROOT}/php-5.5.17
export COMPOSER_HOME=${IROOT}/php-composer
export PHP_FPM=${PHP_HOME}/sbin/php-fpm
export NGINX_HOME=${IROOT}/nginx

sed -i 's|192.168.100.102|'"${DBHOST}"'|g' web/index.php
sed -i 's|".*/FrameworkBenchmarks/php-silex-orm|"'"${TROOT}"'|g' deploy/php-silex-orm
sed -i 's|Directory .*/FrameworkBenchmarks/php-silex-orm|Directory '"${TROOT}"'|g' deploy/php-silex-orm
sed -i 's|root .*/FrameworkBenchmarks/php-silex-orm|root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

export PATH="$COMPOSER_HOME:$PHP_HOME/bin:$PHP_HOME/sbin:$PATH"

$PHP_FPM --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
$NGINX_HOME/sbin/nginx -c $TROOT/deploy/nginx.conf