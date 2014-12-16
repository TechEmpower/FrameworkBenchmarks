#!/bin/bash

sed -i 's|host = "localhost"|host = "'"${DBHOST}"'"|g' application/configs/application.ini
sed -i 's|root .*/FrameworkBenchmarks/php-zend-framework1|root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

export PATH="$COMPOSER_HOME:$PHP_HOME/bin:$PHP_HOME/sbin:$PATH"

composer.phar install
$PHP_FPM --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
$NGINX_HOME/sbin/nginx -c $TROOT/deploy/nginx.conf