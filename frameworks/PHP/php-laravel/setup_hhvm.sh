#!/bin/bash

export PHP_HOME=${IROOT}/php-5.5.17
export COMPOSER_HOME=${IROOT}/php-composer
export PHP_FPM=${PHP_HOME}/sbin/php-fpm
export NGINX_HOME=${IROOT}/nginx

sed -i 's|127.0.0.1|'"${DBHOST}"'|g' app/config/database.php
sed -i 's|SourceRoot = .*/FrameworkBenchmarks/php-laravel|SourceRoot = '"${TROOT}"'|g' deploy/config.hdf
sed -i 's|Path = .*/.hhvm.hhbc|Path = '"${TROOT}"'/.hhvm.bbhc|g' deploy/config.hdf
sed -i 's|PidFile = .*/hhvm.pid|PidFile = '"${TROOT}"'/hhvm.pid|g' deploy/config.hdf
sed -i 's|File = .*/error.log|File = '"${TROOT}"'/error.log|g' deploy/config.hdf
sed -i 's|root .*/FrameworkBenchmarks/frameworks/PHP/php-laravel|root '"${TROOT}"'|g' deploy/nginx.conf 
sed -i 's|/home/vagrant/FrameworkBenchmarks/installs/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

export PATH="${PHP_HOME}/bin:${PHP_HOME}/sbin:$PATH"

hhvm -m daemon --config $TROOT/deploy/config.hdf --user $(whoami)
${NGINX_HOME}/sbin/nginx -c ${TROOT}/deploy/nginx.conf
