#!/bin/bash

sed -i "s|localhost|${DBHOST}|g" app/Conf/config.php
sed -i "s|root .*/FrameworkBenchmarks/php-senthot|root ${TROOT}|g" deploy/nginx.conf
sed -i "s|/usr/local/nginx/|${IROOT}/nginx/|g" deploy/nginx.conf

export PATH="$PHP_HOME/bin:$PHP_HOME/sbin:$PATH"

$PHP_FPM --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
$NGINX_HOME/sbin/nginx -c $TROOT/deploy/nginx.conf