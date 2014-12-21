#!/bin/bash

sed -i "s|localhost|${DBHOST}|g" dborm.php
sed -i "s|localhost|${DBHOST}|g" dbraw.php
sed -i "s|localhost|${DBHOST}|g" updateraw.php
sed -i "s|localhost|${DBHOST}|g" fortune.php

sed -i "s|TEST_ROOT|${TROOT}|g" deploy/php
sed -i "s|TEST_ROOT|${TROOT}|g" deploy/nginx.conf

sed -i "s|/usr/local/nginx/|${IROOT}/nginx/|g" deploy/nginx.conf

export PHP_HOME=${IROOT}/php-5.5.17
export PHP_FPM=$PHP_HOME/sbin/php-fpm
export NGINX_HOME=${IROOT}/nginx

$PHP_FPM --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
$NGINX_HOME/sbin/nginx -c $TROOT/deploy/nginx.conf
