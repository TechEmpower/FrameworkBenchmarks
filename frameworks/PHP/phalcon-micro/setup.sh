#!/bin/bash

fw_depends php nginx

sed -i 's|localhost|'"${DBHOST}"'|g' public/index.php
sed -i 's|root .*/FrameworkBenchmarks/php-phalcon-micro|root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

php-fpm --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
nginx -c $TROOT/deploy/nginx.conf
