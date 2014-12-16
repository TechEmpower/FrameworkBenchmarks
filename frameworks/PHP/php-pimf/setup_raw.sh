#!/bin/bash

sed -i 's|127.0.0.1|'"${DBHOST}"'|g' app/config.app.php
sed -i 's|".*/FrameworkBenchmarks/php-pimf|"'"${TROOT}"'|g' deploy/php-pimf
sed -i 's|Directory .*/FrameworkBenchmarks/php-pimf|Directory '"${TROOT}"'|g' deploy/php-pimf
sed -i 's|root .*/FrameworkBenchmarks/php-pimf|root '"${TROOT}"'|g' deploy/php-pimf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx_raw.conf

export PATH="$COMPOSER_HOME:$PHP_HOME/bin:$PHP_HOME/sbin:$PATH"

composer.phar install --optimize-autoloader
$PHP_FPM --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
$NGINX_HOME/sbin/nginx -c $TROOT/deploy/nginx_raw.conf
