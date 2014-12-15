#!/bin/bash

sed -i 's|192.168.100.102|'"${DBHOST}"'|g' web/app.php
sed -i 's|"./FrameworkBenchmarks/php-silica|"'"${TROOT}"'|g' deploy/php-silica
sed -i 's|Directory .*/FrameworkBenchmarks/php-silica|Directory '"${TROOT}"'|g' deploy/php-silica
sed -i 's|root .*/FrameworkBenchmarks/php-silica|root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

export PATH="$COMPOSER_HOME:$PHP_HOME/bin:$PHP_HOME/sbin:$PATH"

composer.phar install
$PHP_FPM --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
$NGINX_HOME/sbin/nginx -c $TROOT/deploy/nginx.conf