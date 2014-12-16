#!/bin/bash

sed -i 's|host=localhost|host='"${DBHOST}"'|g' config/autoload/benchmarks.local.php
sed -i 'root .*/FrameworkBenchmarks/php-zend-framework|root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

export PATH="$COMPOSER_HOME:$PHP_HOME/bin:$PHP_HOME/sbin:$PATH"

composer.phar install --optimize-autoloader
$PHP_FPM --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
$NGINX_HOME/sbin/nginx -c $TROOT/deploy/nginx.conf 