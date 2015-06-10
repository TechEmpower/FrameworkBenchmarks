#!/bin/bash
export PHP_HOME=${IROOT}/php-5.5.17
export PHP_FPM=$PHP_HOME/sbin/php-fpm
export COMPOSER_HOME=${IROOT}/php-composer
export NGINX_HOME=${IROOT}/nginx

sed -i 's|127.0.0.1|'"${DBHOST}"'|g' app/config.app.php
sed -i 's|".*/FrameworkBenchmarks/php-pimf|"'"${TROOT}"'|g' deploy/php-pimf
sed -i 's|Directory .*/FrameworkBenchmarks/php-pimf|Directory '"${TROOT}"'|g' deploy/php-pimf
sed -i 's|root .*/FrameworkBenchmarks/php-pimf|root '"${TROOT}"'|g' deploy/php-pimf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf
sed -i 's|root .*/FrameworkBenchmarks/php-pimf|root '"${TROOT}"'|g' deploy/nginx.conf

export PATH="$COMPOSER_HOME:$PHP_HOME/bin:$PHP_HOME/sbin:$PATH"

$PHP_FPM --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
$NGINX_HOME/sbin/nginx -c $TROOT/deploy/nginx.conf
