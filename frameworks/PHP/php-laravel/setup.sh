#!/bin/bash

export PHP_HOME=${IROOT}/php-5.5.17
export COMPOSER_HOME=${IROOT}/php-composer
export PHP_FPM=${PHP_HOME}/sbin/php-fpm
export NGINX_HOME=${IROOT}/nginx

sed -i 's|127.0.0.1|'"${DBHOST}"'|g' app/config/database.php
sed -i 's|root .*/FrameworkBenchmarks/frameworks/PHP/php-laravel|root '"${TROOT}"'|g' deploy/nginx.conf 
sed -i 's|/home/vagrant/FrameworkBenchmarks/installs/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

export PATH="${PHP_HOME}/bin:${PHP_HOME}/sbin:$PATH"

$PHP_FPM --fpm-config ${FWROOT}/config/php-fpm.conf -g ${TROOT}/deploy/php-fpm.pid
${NGINX_HOME}/sbin/nginx -c ${TROOT}/deploy/nginx.conf
