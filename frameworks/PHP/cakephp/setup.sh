#!/bin/bash

fw_depends php composer nginx

php-fpm --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid

sed -i "s|'host' => '.*'|'host' => '${DBHOST}'|g" app/Config/database.php
sed -i 's|REDISSERVER|'${DBHOST}'|g' app/Config/core.php
sed -i 's|".*/FrameworkBenchmarks/cakephp|"'${TROOT}'|g' deploy/cake
sed -i 's|Directory .*/FrameworkBenchmarks/cakephp|Directory '${TROOT}'|g' deploy/cake
sed -i 's|root .*/FrameworkBenchmarks/cakephp|root '${TROOT}'|g' deploy/nginx.conf
sed -i 's|/usr/local/nginx/|'${IROOT}'/nginx/|g' deploy/nginx.conf

nginx -c $TROOT/deploy/nginx.conf
