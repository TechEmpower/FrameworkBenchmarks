#!/bin/bash

fw_depends php nginx

sed -i 's|localhost|'"${DBHOST}"'|g' index.php
sed -i 's|.*/FrameworkBenchmarks/fat-free|'"${TROOT}"'|g' deploy/fat-free
sed -i 's|Directory .*/FrameworkBenchmarks/fat-free|Directory '"${TROOT}"'|g' deploy/fat-free
sed -i 's|root .*/FrameworkBenchmarks/fat-free|root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

php-fpm --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
nginx -c $TROOT/deploy/nginx.conf
