#!/bin/bash

fw_depends php nginx composer

sed -i 's|localhost|'"${DBHOST}"'|g' web/index.php
sed -i 's|".*\FrameworkBenchmarks/php-silex|"'"${TROOT}"'|g' deploy/php-silex
sed -i 's|Directory .*/FrameworkBenchmarks/php-silex|Directory '"${TROOT}"'|g' deploy/php-silex
sed -i 's|root .*/FrameworkBenchmarks/php-silex|root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

php-fpm --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
nginx -c $TROOT/deploy/nginx.conf
