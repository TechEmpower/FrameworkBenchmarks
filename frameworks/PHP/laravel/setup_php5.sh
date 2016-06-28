#!/bin/bash

fw_depends php nginx composer

sed -i 's|127.0.0.1|'"${DBHOST}"'|g' app/config/database.php
sed -i 's|root .*/FrameworkBenchmarks/frameworks/PHP/php-laravel|root '"${TROOT}"'|g' deploy/nginx.conf 
sed -i 's|/home/vagrant/FrameworkBenchmarks/installs/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

php-fpm --fpm-config ${FWROOT}/config/php-fpm.conf -g ${TROOT}/deploy/php-fpm.pid
nginx -c ${TROOT}/deploy/nginx.conf
