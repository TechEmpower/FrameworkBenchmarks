#!/bin/bash

sed -i 's|localhost|'"${DBHOST}"'|g' index.php
sed -i 's|root .*/FrameworkBenchmarks/lumen|root '"${TROOT}"'|g' deploy/nginx.conf
sed -i 's|/usr/local/nginx/|'"${IROOT}"'/nginx/|g' deploy/nginx.conf

fw_depends php nginx composer

rm vendor/illuminate/view/FileViewFinder.php
cp modifiedVendorFiles/FileViewFinder.php vendor/illuminate/view/
rm vendor/laravel/lumen-framework/src/Application.php
cp modifiedVendorFiles/Application.php vendor/laravel/lumen-framework/src/
touch vendor/laravel/lumen-framework/storage/logs/lumen.log

php-fpm --fpm-config $FWROOT/config/php-fpm.conf -g $TROOT/deploy/php-fpm.pid
nginx -c $TROOT/deploy/nginx.conf
