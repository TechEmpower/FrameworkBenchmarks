#!/bin/bash

fw_depends mysql php7 nginx composer

sed -i 's|root .*/FrameworkBenchmarks/php-codeigniter|root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

php-fpm --fpm-config $FWROOT/toolset/setup/linux/languages/php/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
nginx -c $TROOT/deploy/nginx.conf
