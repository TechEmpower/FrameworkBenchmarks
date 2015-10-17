#!/bin/bash

fw_depends php nginx composer

sed -i 's|localhost:3306|'"${DBHOST}"':3306|g' index.php
sed -i 's|.*/FrameworkBenchmarks/phreeze|"'"${TROOT}"'|g' deploy/phreeze
sed -i 's|Directory .*/FrameworkBenchmarks/phreeze|Directory '"${TROOT}"'|g' deploy/phreeze
sed -i 's|root .*/FrameworkBenchmarks/phreeze|root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

php-fpm --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
nginx -c $TROOT/deploy/nginx.conf
