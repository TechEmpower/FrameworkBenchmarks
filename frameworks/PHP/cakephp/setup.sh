#!/bin/bash

fw_depends mysql php7 composer nginx

sed -i "s|'host' => '.*'|'host' => '${DBHOST}'|g" app/Config/database.php
sed -i 's|REDISSERVER|'${DBHOST}'|g' app/Config/core.php
sed -i 's|root .*/FrameworkBenchmarks/frameworks/PHP/cakephp|root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/home/vagrant/FrameworkBenchmarks/installs/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

php-fpm --fpm-config $FWROOT/toolset/setup/linux/languages/php/php-fpm.conf -g $TROOT/deploy/php-fpm.pid

nginx -c $TROOT/deploy/nginx.conf
