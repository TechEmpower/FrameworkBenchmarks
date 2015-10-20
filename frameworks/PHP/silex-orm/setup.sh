#!/bin/bash

fw_depends php nginx composer

sed -i 's|192.168.100.102|'"${DBHOST}"'|g' web/index.php
sed -i 's|".*/FrameworkBenchmarks/php-silex-orm|"'"${TROOT}"'|g' deploy/php-silex-orm
sed -i 's|Directory .*/FrameworkBenchmarks/php-silex-orm|Directory '"${TROOT}"'|g' deploy/php-silex-orm
sed -i 's|root .*/FrameworkBenchmarks/php-silex-orm|root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

php-fpm --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
nginx -c $TROOT/deploy/nginx.conf
