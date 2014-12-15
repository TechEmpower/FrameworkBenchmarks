#!/bin/bash

sed -i 's|database_host: .*|database_host: '"${DBHOST}"'|g' app/config/parameters.yml
sed -i 's|root .*/FrameworkBenchmarks/php-symfony2-stripped| root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

export PATH="$COMPOSER_HOME:$PHP_HOME/bin:$PHP_HOME/sbin:$PATH"

composer.phar install --optimize-autoloader
php app/console cache:clear --env=prod --no-debug
php app/console cache:warmup --env=prod --no-debug
$PHP_FPM --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
$NGINX_HOME/sbin/nginx -c $TROOT/deploy/nginx.conf